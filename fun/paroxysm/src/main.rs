// TODO(tazjin): Upgrade to a Diesel version with public derive
// macros.
#[macro_use]
extern crate diesel;

use crate::cfg::Config;
use crate::keyword::KeywordDetails;
use diesel::pg::PgConnection;
use diesel::r2d2::{ConnectionManager, Pool};
use failure::format_err;
use failure::Error;
use irc::client::prelude::*;
use lazy_static::lazy_static;
use log::{debug, info, warn};
use rand::rngs::ThreadRng;
use rand::{thread_rng, Rng};
use regex::{Captures, Regex};
use std::collections::HashMap;
use std::fmt::Display;

mod cfg;
mod keyword;
mod models;
mod schema;

lazy_static! {
    static ref LEARN_RE: Regex =
        Regex::new(r#"^\?\?(?P<gen>!)?\s*(?P<subj>[^\[:]*):\s*(?P<val>.*)"#).unwrap();
    static ref QUERY_RE: Regex =
        Regex::new(r#"^\?\?\s*(?P<subj>[^\[:]*)(?P<idx>\[[^\]]+\])?"#).unwrap();
    static ref QLAST_RE: Regex = Regex::new(r#"^\?\?\s*(?P<subj>[^\[:]*)!"#).unwrap();
    static ref INCREMENT_RE: Regex =
        Regex::new(r#"^\?\?(?P<gen>!)?\s*(?P<subj>[^\[:]*)(?P<incrdecr>\+\+|\-\-)"#).unwrap();
    static ref MOVE_RE: Regex =
        Regex::new(r#"^\?\?(?P<gen>!)?\s*(?P<subj>[^\[:]*)(?P<idx>\[[^\]]+\])->(?P<new_idx>.*)"#)
            .unwrap();
}

pub struct App {
    client: IrcClient,
    pg: Pool<ConnectionManager<PgConnection>>,
    rng: ThreadRng,
    cfg: Config,
    last_msgs: HashMap<String, HashMap<String, String>>,
}

impl App {
    pub fn report_error<T: Display>(
        &mut self,
        nick: &str,
        chan: &str,
        msg: T,
    ) -> Result<(), Error> {
        self.client
            .send_notice(nick, format!("[{}] \x0304Error:\x0f {}", chan, msg))?;
        Ok(())
    }

    pub fn keyword_from_captures(
        &mut self,
        learn: &::regex::Captures,
        nick: &str,
        chan: &str,
    ) -> Result<KeywordDetails, Error> {
        let db = self.pg.get()?;
        debug!("Fetching keyword for captures: {:?}", learn);
        let subj = &learn["subj"];
        let learn_chan = if learn.name("gen").is_some() {
            "*"
        } else {
            chan
        };
        if !chan.starts_with("#") && learn_chan != "*" {
            Err(format_err!("Only general entries may be taught via PM."))?;
        }
        debug!("Fetching keyword '{}' for chan {}", subj, learn_chan);
        let kwd = KeywordDetails::get_or_create(subj, learn_chan, &db)?;
        if kwd.keyword.chan == "*" && !self.cfg.admins.contains(nick) {
            Err(format_err!(
                "Only administrators can create or modify general entries."
            ))?;
        }
        Ok(kwd)
    }

    pub fn handle_move(
        &mut self,
        target: &str,
        nick: &str,
        chan: &str,
        mv: Captures,
    ) -> Result<(), Error> {
        let db = self.pg.get()?;
        let idx = &mv["idx"];
        let idx = match idx[1..(idx.len() - 1)].parse::<usize>() {
            Ok(i) => i,
            Err(e) => Err(format_err!("Could not parse index: {}", e))?,
        };
        let new_idx = match mv["new_idx"].parse::<i32>() {
            Ok(i) => i,
            Err(e) => Err(format_err!("Could not parse target index: {}", e))?,
        };
        let mut kwd = self.keyword_from_captures(&mv, nick, chan)?;
        if new_idx < 0 {
            kwd.delete(idx, &db)?;
            self.client.send_notice(
                target,
                format!("\x02{}\x0f: Deleted entry {}.", kwd.keyword.name, idx),
            )?;
        } else {
            kwd.swap(idx, new_idx as _, &db)?;
            self.client.send_notice(
                target,
                format!(
                    "\x02{}\x0f: Swapped entries {} and {}.",
                    kwd.keyword.name, idx, new_idx
                ),
            )?;
        }
        Ok(())
    }

    pub fn handle_learn(
        &mut self,
        target: &str,
        nick: &str,
        chan: &str,
        learn: Captures,
    ) -> Result<(), Error> {
        let db = self.pg.get()?;
        let val = &learn["val"];
        let mut kwd = self.keyword_from_captures(&learn, nick, chan)?;
        let idx = kwd.learn(nick, val, &db)?;
        self.client
            .send_notice(target, kwd.format_entry(idx).unwrap())?;
        Ok(())
    }

    pub fn handle_insert_last_quote(
        &mut self,
        target: &str,
        nick: &str,
        chan: &str,
        qlast: Captures,
    ) -> Result<(), Error> {
        let db = self.pg.get()?;
        let nick_to_grab = &qlast["subj"].to_ascii_lowercase();
        let mut kwd = self.keyword_from_captures(&qlast, nick, chan)?;
        let chan_lastmsgs = self
            .last_msgs
            .entry(chan.to_string())
            .or_insert(HashMap::new());
        // Use `nick` here, so things like "grfn: see glittershark" work.
        let val = if let Some(last) = chan_lastmsgs.get(nick_to_grab) {
            if last.starts_with("\x01ACTION ") {
                // Yes, this is inefficient, but it's better than writing some hacky CTCP parsing code
                // I guess (also, characters are hard, so just blindly slicing seems like a bad idea)
                format!(
                    "* {} {}",
                    nick_to_grab,
                    last.replace("\x01ACTION ", "").replace("\x01", "")
                )
            } else {
                format!("<{}> {}", nick_to_grab, last)
            }
        } else {
            Err(format_err!("I dunno what {} said...", kwd.keyword.name))?
        };
        let idx = kwd.learn(nick, &val, &db)?;
        self.client
            .send_notice(target, kwd.format_entry(idx).unwrap())?;
        Ok(())
    }

    pub fn handle_increment(
        &mut self,
        target: &str,
        nick: &str,
        chan: &str,
        icr: Captures,
    ) -> Result<(), Error> {
        let db = self.pg.get()?;
        let mut kwd = self.keyword_from_captures(&icr, nick, chan)?;
        let is_incr = &icr["incrdecr"] == "++";
        let now = chrono::Utc::now().naive_utc().date();
        let mut idx = None;
        for (i, ent) in kwd.entries.iter().enumerate() {
            if ent.creation_ts.date() == now {
                if let Ok(val) = ent.text.parse::<i32>() {
                    let val = if is_incr { val + 1 } else { val - 1 };
                    idx = Some((i + 1, val));
                }
            }
        }
        if let Some((i, val)) = idx {
            kwd.update(i, &val.to_string(), &db)?;
            self.client
                .send_notice(target, kwd.format_entry(i).unwrap())?;
        } else {
            let val = if is_incr { 1 } else { -1 };
            let idx = kwd.learn(nick, &val.to_string(), &db)?;
            self.client
                .send_notice(target, kwd.format_entry(idx).unwrap())?;
        }
        Ok(())
    }

    pub fn handle_query(
        &mut self,
        target: &str,
        nick: &str,
        chan: &str,
        query: Captures,
    ) -> Result<(), Error> {
        let db = self.pg.get()?;
        let subj = &query["subj"];
        let idx = match query.name("idx") {
            Some(i) => {
                let i = i.as_str();
                match &i[1..(i.len() - 1)] {
                    "*" => Some(-1),
                    x => x.parse::<usize>().map(|x| x as i32).ok(),
                }
            }
            None => None,
        };
        debug!("Querying {} with idx {:?}", subj, idx);
        match KeywordDetails::get(subj, chan, &db)? {
            Some(kwd) => {
                if let Some(mut idx) = idx {
                    if idx == -1 {
                        // 'get all entries' ('*' parses into this)
                        // step 1: make a blob of all the quotes
                        let mut data_to_upload = String::new();
                        for i in 0..kwd.entries.len() {
                            data_to_upload
                                .push_str(&kwd.format_entry_colours(i + 1, false).unwrap());
                            data_to_upload.push('\n');
                        }
                        // step 2: attempt to POST it to eta's pastebin
                        // TODO(eta): make configurable
                        let response = crimp::Request::put("https://eta.st/lx/upload")
                            .user_agent("paroxysm/0.0.2 crimp/0.2")?
                            .header("Linx-Expiry", "7200")? // 2 hours
                            .body("text/plain", data_to_upload.as_bytes())
                            .timeout(std::time::Duration::from_secs(2))?
                            .send()?
                            .as_string()?;
                        // step 3: tell the world about it
                        if response.status != 200 {
                            Err(format_err!(
                                "upload returned {}: {}",
                                response.status,
                                response.body
                            ))?
                        }
                        self.client.send_notice(
                            target,
                            format!(
                                "\x02{}\x0f: uploaded {} quotes to \x02\x0311{}\x0f (will expire in \x0224\x0f hours)",
                                subj,
                                kwd.entries.len(),
                                response.body
                            )
                        )?;
                    } else {
                        if idx == 0 {
                            idx = 1;
                        }
                        if let Some(ent) = kwd.format_entry(idx as _) {
                            self.client.send_notice(target, ent)?;
                        } else {
                            let pluralised = if kwd.entries.len() == 1 {
                                "entry"
                            } else {
                                "entries"
                            };
                            self.client.send_notice(
                                target,
                                format!(
                                    "\x02{}\x0f: only has \x02\x0304{}\x0f {}",
                                    subj,
                                    kwd.entries.len(),
                                    pluralised
                                ),
                            )?;
                        }
                    }
                } else {
                    let entry = if kwd.entries.len() < 2 {
                        1 // because [1, 1) does not a range make
                    } else {
                        self.rng.gen_range(1, kwd.entries.len())
                    };
                    if let Some(ent) = kwd.format_entry(entry) {
                        self.client.send_notice(target, ent)?;
                    } else {
                        self.client
                            .send_notice(target, format!("\x02{}\x0f: no entries yet", subj))?;
                    }
                }
            }
            None => {
                self.client
                    .send_notice(target, format!("\x02{}\x0f: never heard of it", subj))?;
            }
        }
        Ok(())
    }

    pub fn handle_privmsg(&mut self, from: &str, chan: &str, msg: &str) -> Result<(), Error> {
        let nick = from.split("!").next().ok_or(format_err!(
            "Received PRIVMSG from a source without nickname (failed to split n!u@h)"
        ))?;
        let target = if chan.starts_with("#") { chan } else { nick };
        debug!("[{}] <{}> {}", chan, nick, msg);
        if let Some(learn) = LEARN_RE.captures(msg) {
            self.handle_learn(target, nick, chan, learn)?;
        } else if let Some(qlast) = QLAST_RE.captures(msg) {
            self.handle_insert_last_quote(target, nick, chan, qlast)?;
        } else if let Some(mv) = MOVE_RE.captures(msg) {
            self.handle_move(target, nick, chan, mv)?;
        } else if let Some(icr) = INCREMENT_RE.captures(msg) {
            self.handle_increment(target, nick, chan, icr)?;
        } else if let Some(query) = QUERY_RE.captures(msg) {
            self.handle_query(target, nick, chan, query)?;
        } else {
            let chan_lastmsgs = self
                .last_msgs
                .entry(chan.to_string())
                .or_insert(HashMap::new());
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
            }
            Command::INVITE(nick, channel) => {
                if self.cfg.admins.contains(&nick) {
                    info!("Joining {} after admin invite", channel);
                    self.client.send_join(channel)?;
                }
            }
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
    let env = env_logger::Env::new()
        .default_filter_or(cfg.log_filter.clone().unwrap_or(default_log_filter));
    env_logger::init_from_env(env);
    info!("paroxysm starting up");
    info!("connecting to database at {}", cfg.database_url);
    let pg = Pool::new(ConnectionManager::new(&cfg.database_url))?;
    info!("connecting to IRC using config {}", cfg.irc_config_path);
    let client = IrcClient::new(&cfg.irc_config_path)?;
    client.identify()?;
    let st = client.stream();
    let mut app = App {
        client,
        pg,
        cfg,
        rng: thread_rng(),
        last_msgs: HashMap::new(),
    };
    info!("running!");
    st.for_each_incoming(|m| {
        if let Err(e) = app.handle_msg(m) {
            warn!("Error processing message: {}", e);
        }
    })?;
    Ok(())
}
