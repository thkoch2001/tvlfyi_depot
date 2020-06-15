extern crate irc;
extern crate serde;
#[macro_use] extern crate serde_derive;
#[macro_use] extern crate diesel;
extern crate chrono;
extern crate config;
extern crate env_logger;
#[macro_use] extern crate log;
#[macro_use] extern crate failure;
extern crate regex;
#[macro_use] extern crate lazy_static;
extern crate rand;

use irc::client::prelude::*;
use diesel::pg::PgConnection;
use diesel::r2d2::{ConnectionManager, Pool};
use failure::Error;
use regex::{Captures, Regex};
use std::fmt::Display;
use crate::cfg::Config;
use crate::keyword::KeywordDetails;
use rand::{thread_rng, Rng};
use rand::rngs::ThreadRng;
use std::collections::HashMap;

mod schema;
mod models;
mod cfg;
mod keyword;

pub struct App {
    cli: IrcClient,
    pg: Pool<ConnectionManager<PgConnection>>,
    rng: ThreadRng,
    cfg: Config,
    last_msgs: HashMap<String, HashMap<String, String>>
}

impl App {
    pub fn report_error<T: Display>(&mut self, nick: &str, chan: &str, msg: T) -> Result<(), Error> {
        self.cli.send_notice(nick, format!("[{}] \x0304Error:\x0f {}", chan, msg))?;
        Ok(())
    }
    pub fn keyword_from_captures(&mut self, learn: &::regex::Captures, nick: &str, chan: &str) -> Result<KeywordDetails, Error> {
        let db = self.pg.get()?;
        debug!("Fetching keyword for captures: {:?}", learn);
        let subj = &learn["subj"];
        let learn_chan = if learn.name("gen").is_some() {
            "*"
        }
        else {
            chan
        };
        if !chan.starts_with("#") && learn_chan != "*" {
            Err(format_err!("Only general entries may be taught via PM."))?;
        }
        debug!("Fetching keyword '{}' for chan {}", subj, learn_chan);
        let kwd = KeywordDetails::get_or_create(subj, learn_chan, &db)?;
        if kwd.keyword.chan == "*" && !self.cfg.admins.contains(nick) {
            Err(format_err!("Only administrators can create or modify general entries."))?;
        }
        Ok(kwd)
    }
    pub fn handle_move(&mut self, target: &str, nick: &str, chan: &str, mv: Captures) -> Result<(), Error> {
        let db = self.pg.get()?;
        let idx = &mv["idx"];
        let idx = match idx[1..(idx.len()-1)].parse::<usize>() {
            Ok(i) => i,
            Err(e) => {
                Err(format_err!("Could not parse index: {}", e))?
            }
        };
        let new_idx = match mv["new_idx"].parse::<i32>() {
            Ok(i) => i,
            Err(e) => {
                Err(format_err!("Could not parse target index: {}", e))?
            }
        };
        let mut kwd = self.keyword_from_captures(&mv, nick, chan)?;
        if new_idx < 0 {
            kwd.delete(idx, &db)?;
            self.cli.send_notice(target, format!("\x02{}\x0f: Deleted entry {}.", kwd.keyword.name, idx))?;
        }
        else {
            kwd.swap(idx, new_idx as _, &db)?;
            self.cli.send_notice(target, format!("\x02{}\x0f: Swapped entries {} and {}.", kwd.keyword.name, idx, new_idx))?;
        }
        Ok(())
    }
    pub fn handle_learn(&mut self, target: &str, nick: &str, chan: &str, learn: Captures) -> Result<(), Error> {
        let db = self.pg.get()?;
        let val = &learn["val"];
        let mut kwd = self.keyword_from_captures(&learn, nick, chan)?;
        let idx = kwd.learn(nick, val, &db)?;
        self.cli.send_notice(target, kwd.format_entry(idx).unwrap())?;
        Ok(())
    }
    pub fn handle_insert_last_quote(&mut self, target: &str, nick: &str, chan: &str, qlast: Captures) -> Result<(), Error> {
        let db = self.pg.get()?;
        let mut kwd = self.keyword_from_captures(&qlast, nick, chan)?;
        let chan_lastmsgs = self.last_msgs.entry(chan.to_string()).or_insert(HashMap::new());
        let val = if let Some(last) = chan_lastmsgs.get(&kwd.keyword.name.to_ascii_lowercase()) {
            format!("<{}> {}", &kwd.keyword.name, last)
        }
        else {
            Err(format_err!("I dunno what {} said...", kwd.keyword.name))?
        };
        let idx = kwd.learn(nick, &val, &db)?;
        self.cli.send_notice(target, kwd.format_entry(idx).unwrap())?;
        Ok(())
    }
    pub fn handle_increment(&mut self, target: &str, nick: &str, chan: &str, icr: Captures) -> Result<(), Error> {
        let db = self.pg.get()?;
        let mut kwd = self.keyword_from_captures(&icr, nick, chan)?;
        let is_incr = &icr["incrdecr"] == "++";
        let now = chrono::Utc::now().naive_utc().date();
        let mut idx = None;
        for (i, ent) in kwd.entries.iter().enumerate() {
            if ent.creation_ts.date() == now {
                if let Ok(val) = ent.text.parse::<i32>() {
                    let val = if is_incr {
                        val + 1
                    }
                    else {
                        val - 1
                    };
                    idx = Some((i+1, val));
                }
            }
        }
        if let Some((i, val)) = idx {
            kwd.update(i, &val.to_string(), &db)?;
            self.cli.send_notice(target, kwd.format_entry(i).unwrap())?;
        }
        else {
            let val = if is_incr { 1 } else { -1 };
            let idx = kwd.learn(nick, &val.to_string(), &db)?;
            self.cli.send_notice(target, kwd.format_entry(idx).unwrap())?;
        }
        Ok(())
    }
    pub fn handle_query(&mut self, target: &str, nick: &str, chan: &str, query: Captures) -> Result<(), Error> {
        let db = self.pg.get()?;
        let subj = &query["subj"];
        let idx = match query.name("idx") {
            Some(i) => {
                let i = i.as_str();
                match &i[1..(i.len()-1)] {
                    "*" => Some(-1),
                    x => x.parse::<usize>().map(|x| x as i32).ok(),
                }
            },
            None => None,
        };
        debug!("Querying {} with idx {:?}", subj, idx);
        match KeywordDetails::get(subj, chan, &db)? {
            Some(kwd) => {
                if let Some(mut idx) = idx {
                    if idx == -1 {
                        // 'get all entries' ('*' parses into this)
                        for i in 0..kwd.entries.len() {
                            self.cli.send_notice(nick, format!("[{}] {}", chan, kwd.format_entry(i+1).unwrap()))?;
                        }
                    }
                    else {
                        if idx == 0 {
                            idx = 1;
                        }
                        if let Some(ent) = kwd.format_entry(idx as _) {
                            self.cli.send_notice(target, ent)?;
                        }
                        else {
                            let pluralised = if kwd.entries.len() == 1 {
                                "entry"
                            }
                            else {
                                "entries"
                            };
                            self.cli.send_notice(target, format!("\x02{}\x0f: only has \x02\x0304{}\x0f {}", subj, kwd.entries.len(), pluralised))?;
                        }
                    }
                }
                else {
                    let entry = if kwd.entries.len() < 2 {
                        1 // because [1, 1) does not a range make
                    }
                    else {
                        self.rng.gen_range(1, kwd.entries.len())
                    };
                    if let Some(ent) = kwd.format_entry(entry) {
                        self.cli.send_notice(target, ent)?;
                    }
                    else {
                        self.cli.send_notice(target, format!("\x02{}\x0f: no entries yet", subj))?;
                    }
                }
            },
            None => {
                self.cli.send_notice(target, format!("\x02{}\x0f: never heard of it", subj))?;
            }
        }
        Ok(())
    }
    pub fn handle_privmsg(&mut self, from: &str, chan: &str, msg: &str) -> Result<(), Error> {
        lazy_static! {
            static ref LEARN_RE: Regex = Regex::new(r#"^\?\?(?P<gen>!)?\s*(?P<subj>[^\[:]*):\s*(?P<val>.*)"#).unwrap();
            static ref QUERY_RE: Regex = Regex::new(r#"^\?\?\s*(?P<subj>[^\[:]*)(?P<idx>\[[^\]]+\])?"#).unwrap();
            static ref QLAST_RE: Regex = Regex::new(r#"^\?\?\s*(?P<subj>[^\[:]*)!"#).unwrap();
            static ref INCREMENT_RE: Regex = Regex::new(r#"^\?\?(?P<gen>!)?\s*(?P<subj>[^\[:]*)(?P<incrdecr>\+\+|\-\-)"#).unwrap();
            static ref MOVE_RE: Regex = Regex::new(r#"^\?\?(?P<gen>!)?\s*(?P<subj>[^\[:]*)(?P<idx>\[[^\]]+\])->(?P<new_idx>.*)"#).unwrap();
        }
        let nick = from.split("!").next().ok_or(format_err!("Received PRIVMSG from a source without nickname (failed to split n!u@h)"))?;
        let target = if chan.starts_with("#") {
            chan
        }
        else {
            nick
        };
        debug!("[{}] <{}> {}", chan, nick, msg);
        if let Some(learn) = LEARN_RE.captures(msg) {
            self.handle_learn(target, nick, chan, learn)?;
        }
        else if let Some(qlast) = QLAST_RE.captures(msg) {
            self.handle_insert_last_quote(target, nick, chan, qlast)?;
        }
        else if let Some(mv) = MOVE_RE.captures(msg) {
            self.handle_move(target, nick, chan, mv)?;
        }
        else if let Some(icr) = INCREMENT_RE.captures(msg) {
            self.handle_increment(target, nick, chan, icr)?;
        }
        else if let Some(query) = QUERY_RE.captures(msg) {
            self.handle_query(target, nick, chan, query)?;
        }
        else {
            let chan_lastmsgs = self.last_msgs.entry(chan.to_string()).or_insert(HashMap::new());
            chan_lastmsgs.insert(nick.to_string().to_ascii_lowercase(), msg.to_string());
        }
        Ok(())
    }
    pub fn handle_msg(&mut self, m: Message) -> Result<(), Error> {
        match m.command {
            Command::PRIVMSG(channel, message) => {
                if let Some(src) = m.prefix {
                    if let Err(e) = self.handle_privmsg(&src, &channel, &message) {
                        warn!("error handling command in {} (src {}): {}", channel, src, e);
                        if let Some(nick) = src.split("!").next() {
                            self.report_error(nick, &channel, e)?;
                        }
                    }
                }
            },
            Command::INVITE(nick, channel) => {
                if self.cfg.admins.contains(&nick) {
                    info!("Joining {} after admin invite", channel);
                    self.cli.send_join(channel)?;
                }
            },
            _ => {}
        }
        Ok(())
    }
}
fn main() -> Result<(), Error> {
    println!("[+] loading configuration");
    let default_log_filter = "paroxysm=info".to_string();
    let mut settings = config::Config::default();
    settings.merge(config::Environment::with_prefix("PARX"))?;
    let cfg: Config = settings.try_into()?;
    let env = env_logger::Env::new().default_filter_or(cfg.log_filter.clone().unwrap_or(default_log_filter));
    env_logger::init_from_env(env);
    info!("paroxysm starting up");
    info!("connecting to database");
    let pg = Pool::new(ConnectionManager::new(&cfg.database_url))?;
    info!("connecting to IRC");
    let cli = IrcClient::new(&cfg.irc_config_path)?;
    cli.identify()?;
    let st = cli.stream();
    let mut app = App { cli, pg, cfg, rng: thread_rng(), last_msgs: HashMap::new() };
    info!("running!");
    st.for_each_incoming(|m| {
        if let Err(e) = app.handle_msg(m) {
            warn!("Error processing message: {}", e);
        }
    })?;
    Ok(())
}
