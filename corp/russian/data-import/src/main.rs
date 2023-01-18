//! This program imports Russian language data from OpenCorpora
//! ("Открытый корпус") into a SQLite database that can be used for
//! [//corp/russian][corp-russian] projects.
//!
//! [corp-russian]: https://at.tvl.fyi/?q=%2F%2Fcorp%2Frussian
//!
//! Ideally, running this on an OpenCorpora dump should yield a fully
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

use log::{error, info};
use rusqlite::{Connection, Result};
use std::env;
use std::fmt::Display;
use std::fs::File;
use std::io::BufReader;

mod db_setup;
mod oc_parser;

fn main() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Info)
        .init();

    let input_path = env::args()
        .skip(1)
        .next()
        .ensure("must specify the input filename as the only argument");

    info!("reading from {input_path}");
    let input_file = File::open(input_path).ensure("failed to open input file");

    let mut parser = oc_parser::OpenCorporaParser::new(BufReader::new(input_file));

    let conn = Connection::open("out.db").ensure("failed to open DB connection");

    db_setup::initial_schema(&conn);

    // afterwards:
    // add actual IDs to grammemes
    // properly reference keys internally
    // add foreign key constraint on lemma_grammemes.grammeme

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
