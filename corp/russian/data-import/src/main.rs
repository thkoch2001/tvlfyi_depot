//! This program imports Russian language data from OpenCorpora
//! ("Открытый корпус") and OpenRussian into a SQLite database that
//! can be used for [//corp/russian][corp-russian] projects.
//!
//! [corp-russian]: https://at.tvl.fyi/?q=%2F%2Fcorp%2Frussian
//!
//! Ideally, running this on intact dumps should yield a fully
//! functional SQLite database compatible with all other tools
//! consuming it.
//!
//! ## OpenCorpora format
//!
//! The format used is partially documented on the [OpenCorpora
//! website][format-docs]. This seems to be a slightly outdated
//! format, however, hence some information about what the format
//! seems to be today.
//!
//! [format-docs]: http://opencorpora.org/?page=export
//!
//! The format is an XML file, which has several categories of data,
//! each with their own schema:
//!
//! * `grammemes`: These define units of grammar. They're *likely* pretty
//!   static, and we'll *likely* want to map them into a custom set of
//!   (simpler) categories.
//!
//!   They form some kind of internal hierarchy, where some of them have a
//!   `parent` attribute set to some other grammemes `name`.
//!
//!   There's a ridiculous number of these.
//!
//! * `restrictions`: Unclear, not documented on the page. They describe
//!   something about the relationship between grammemes.
//!
//! * `lemmata`: this lists the actual lemmas, as well as all their
//!   included morphological variants
//!
//!   Each lemma has an `id` attribute uniquely identifying its dictionary
//!   form, as well as a number of sub-elements:
//!
//!   * the `l` attribute contains the lemma itself
//!   * the `f` attributes contain morphological variations
//!
//!   Each of these sub elements again contains a number of `g` elements,
//!   which refer to the IDs of grammems in their `v` attributes.
//!
//! * `<link_types>` These list possible "relationships between lemmas",
//!   basically just assigning them IDs and names. There's only 27 of
//!   these.
//!
//! * `<links>`: Using the types defined above, this establishes links
//!   between lemmas that have some kind of relationship.
//!
//!   For example, a relationship `cardinal/ordinal` might be established
//!   between the lemmas "два" and "второй".
//!
//! ## OpenRussian format
//!
//! The [OpenRussian](https://en.openrussian.org/dictionary) project
//! lets users export its database as a set of CSV-files. For our
//! purposes, we download the files using `<tab>` separators.
//!
//! Whereas OpenCorpora opts for a flat structure with a "tag" system
//! (through its flexible grammemes), OpenRussian has a fixed pre-hoc
//! structure into which it sorts some words with their morphologies.
//! The OpenRussian database is much smaller as of January 2023 (~1.7
//! million words vs. >5 million for OpenCorpora), but some of the
//! information is much more practically useful.
//!
//! Two very important bits of information OpenRussian has are accent
//! marks (most tables containing actual words have a normal form
//! containing and accent mark, and a "bare" form without) and
//! translations into English and German.
//!
//! The full dump includes the following tables (and some more):
//!
//! * `words`: List of lemmas in the corpus, with various bits of
//!    metadata as well as hand-written notes.
//!
//! * `adjectives`: Contains IDs for words that are adjectives.
//!
//! * `nouns`: IDs for words that are nouns; and noun metadata (e.g.
//!   gender, declinability)
//!
//! * `verbs`: IDs of words that are verbs, including their aspect and
//!   "partnered" verb in the other aspect
//!
//! * `words_forms`: Contains all morphed variants of the lemmas from
//!   `words`, including information about their grammeme, and accent
//!   marks.
//!
//! * `words_rels`: Contains relations between words, containing
//!   information like "synonyms" or general relation between words.
//!
//! * `translations`: Contains translations tagged by target language,
//!   as well as examples and (occasionally) additional information.
//!
//! These tables also contain something, but have not been analysed
//! yet:
//!
//! * `expressions_words`
//! * `sentences`
//! * `sentences_translations`
//! * `sentences_words`

use log::{error, info};
use rusqlite::{Connection, Result};
use std::env;
use std::fmt::Display;
use std::fs::File;
use std::io::BufReader;

mod db_setup;
mod mappings;
mod oc_parser;
mod or_parser;

struct Args {
    output: String,
    or_input: String,
    oc_input: String,
}

impl Args {
    fn populated(&self) -> bool {
        !(self.output.is_empty() || self.or_input.is_empty() || self.oc_input.is_empty())
    }
}

fn usage(binary_name: &str) {
    bail(format!(
        "usage: {} --output <output-file> --or-input <or-input> --oc-input <oc-input>",
        binary_name
    ));
}

fn parse_args() -> Args {
    let mut args_iter = env::args();
    let binary_name = args_iter.next().unwrap();

    let mut args = Args {
        output: "".into(),
        or_input: env::var("OPENRUSSIAN_DATA").unwrap_or_default(),
        oc_input: env::var("OPENCORPORA_DATA").unwrap_or_default(),
    };

    loop {
        if args.populated() {
            break;
        }

        while let Some(arg) = args_iter.next() {
            match arg.as_str() {
                "--output" => {
                    args.output = args_iter.next().unwrap();
                }

                "--or-input" => {
                    args.or_input = args_iter.next().unwrap();
                }

                "--oc-input" => {
                    args.oc_input = args_iter.next().unwrap();
                }

                _ => usage(&binary_name),
            }
        }
    }

    if args.output.is_empty() || args.or_input.is_empty() || args.oc_input.is_empty() {
        usage(&binary_name);
    }

    args
}

fn open_corpora(conn: &Connection, args: &Args) {
    let input_file = File::open(&args.oc_input).ensure("failed to open input file");
    let mut parser = oc_parser::OpenCorporaParser::new(BufReader::new(input_file));
    db_setup::initial_oc_schema(&conn);

    let mut tx = conn
        .unchecked_transaction()
        .ensure("failed to start transaction");

    let mut count = 0;

    while let Some(elem) = parser.next_element() {
        // commit every 1000 things
        if count % 1000 == 0 {
            tx.commit().ensure("transaction failed");
            tx = conn
                .unchecked_transaction()
                .ensure("failed to start new transaction");
            info!("transaction committed at watermark {}", count);
        }

        db_setup::insert_oc_element(&tx, elem);

        count += 1;
    }

    tx.commit().ensure("final OpenCorpora commit failed");

    info!("finished OpenCorpora import");
}

fn open_russian(conn: &Connection, args: &Args) {
    let parser = or_parser::OpenRussianParser::new(&args.or_input);

    db_setup::initial_or_schema(conn);

    {
        let tx = conn
            .unchecked_transaction()
            .ensure("failed to start transaction");

        db_setup::insert_or_words(&tx, parser.words());
        tx.commit().ensure("OpenRussian words commit failed");
    }

    {
        let tx = conn
            .unchecked_transaction()
            .ensure("failed to start transaction");

        db_setup::insert_or_word_forms(&tx, parser.words_forms());
        tx.commit().ensure("OpenRussian word forms commit failed");
    }

    {
        let tx = conn
            .unchecked_transaction()
            .ensure("failed to start transaction");

        db_setup::insert_or_translations(&tx, parser.translations());
        tx.commit().ensure("OpenRussian translations commit failed");
    }

    info!("finished OpenRussian import");
}

fn main() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Info)
        .init();

    let args = parse_args();

    info!("output path: {}", args.output);
    info!("OpenCorpora input path: {}", args.oc_input);
    info!("OpenRussian input path: {}", args.or_input);

    let conn = Connection::open(&args.output).ensure("failed to open DB connection");

    open_corpora(&conn, &args);
    open_russian(&conn, &args);

    // afterwards:
    // add actual IDs to grammemes
    // properly reference keys internally
    // add foreign key constraint on lemma_grammemes.grammeme
}

/// It's like `expect`, but through `log::error`.
trait Ensure<T> {
    fn ensure<S: Into<String>>(self, msg: S) -> T;
}

impl<T, E: Display> Ensure<T> for Result<T, E> {
    fn ensure<S: Into<String>>(self, msg: S) -> T {
        match self {
            Ok(x) => x,
            Err(err) => {
                error!("{}: {}", msg.into(), err);
                std::process::exit(1);
            }
        }
    }
}

impl<T> Ensure<T> for Option<T> {
    fn ensure<S: Into<String>>(self, msg: S) -> T {
        match self {
            Some(x) => x,
            None => {
                error!("{}", msg.into());
                std::process::exit(1);
            }
        }
    }
}

fn bail<S: Into<String>>(msg: S) -> ! {
    error!("{}", msg.into());
    std::process::exit(1);
}
