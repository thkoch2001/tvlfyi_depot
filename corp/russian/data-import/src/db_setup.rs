//! This module prepares the database layout.
//!
//! The XML import may be in an arbitrary order, so importing data is
//! a multi-step process where we first set up schemas matching the
//! data layout, import the data, and then modify the schema to
//! introduce things like foreign key constraints between tables that
//! represent relations.

use super::Ensure;
use crate::oc_parser::*;
use crate::or_parser;
use log::{debug, info};
use rusqlite::Connection;

/// Sets up an initial schema which matches the OpenCorpora data.
pub fn initial_oc_schema(conn: &Connection) {
    conn.execute_batch(
        r#"
-- table for plain import of grammemes from XML
CREATE TABLE oc_grammemes (
    name TEXT PRIMARY KEY,
    parent TEXT,
    alias TEXT,
    description TEXT
) STRICT;

-- table for plain import of lemmas (*not* their variations!)
CREATE TABLE oc_lemmas (
    id INTEGER PRIMARY KEY,
    lemma TEXT NOT NULL
) STRICT;

-- table for relationship between grammemes and lemmas
CREATE TABLE oc_lemma_grammemes (
    lemma INTEGER,
    grammeme TEXT NOT NULL,
    FOREIGN KEY(lemma) REFERENCES oc_lemmas(id)
) STRICT;

-- table for all words, i.e. including variations of lemmata
CREATE TABLE oc_words (
    lemma INTEGER NOT NULL,
    word TEXT NOT NULL,
    FOREIGN KEY(lemma) REFERENCES oc_lemmas(id)
) STRICT;

-- table for relationship between words and grammemes
CREATE TABLE oc_word_grammemes (
    word INTEGER NOT NULL,
    grammeme TEXT NOT NULL,
    FOREIGN KEY(word) REFERENCES oc_words(ROWID)
) STRICT;

-- table for link types
CREATE TABLE oc_link_types (
  id INTEGER PRIMARY KEY,
  name TEXT
) STRICT;

-- table for links between lemmata
CREATE TABLE oc_links (
  id INTEGER PRIMARY KEY,
  link_type INTEGER NOT NULL,
  from_lemma INTEGER NOT NULL,
  to_lemma INTEGER NOT NULL,
  FOREIGN KEY(link_type) REFERENCES oc_link_types(id),
  FOREIGN KEY(from_lemma) REFERENCES oc_lemmas(id),
  FOREIGN KEY(to_lemma) REFERENCES oc_lemmas(id)
) STRICT;

"#,
    )
    .ensure("setting up OpenCorpora table schema failed");

    info!("set up initial table schema for OpenCorpora import");
}

/// Inserts a single OpenCorpora element into the initial table structure.
pub fn insert_oc_element(conn: &Connection, elem: OcElement) {
    match elem {
        OcElement::Grammeme(grammeme) => {
            conn.execute(
                "INSERT INTO oc_grammemes (name, parent, alias, description) VALUES (?1, ?2, ?3, ?4)",
                (
                    &grammeme.name,
                    &grammeme.parent,
                    &grammeme.alias,
                    &grammeme.description,
                ),
            )
            .ensure("failed to insert grammeme");

            debug!("inserted grammeme {}", grammeme.name);
        }

        OcElement::Lemma(lemma) => insert_lemma(conn, lemma),

        OcElement::LinkType(lt) => {
            conn.execute(
                "INSERT INTO oc_link_types (id, name) VALUES (?1, ?2)",
                (&lt.id, &lt.name),
            )
            .ensure("failed to insert link type");

            info!("inserted link type {}", lt.name);
        }

        OcElement::Link(link) => {
            let mut stmt = conn
                .prepare_cached(
                    "INSERT INTO oc_links (id, link_type, from_lemma, to_lemma) VALUES (?1, ?2, ?3, ?4)",
                )
                .ensure("failed to prepare link statement");

            stmt.execute((&link.id, &link.link_type, &link.from, &link.to))
                .ensure("failed to insert link");

            debug!("inserted link {}", link.id);
        }
    }
}

/// Insert a single lemma into the initial structure. This is somewhat
/// involved because it also establishes a bunch of relations.
fn insert_lemma(conn: &Connection, lemma: Lemma) {
    // insert the lemma itself
    let mut stmt = conn
        .prepare_cached("INSERT INTO oc_lemmas (id, lemma) VALUES (?1, ?2)")
        .ensure("failed to prepare statement");

    stmt.execute((&lemma.id, &lemma.lemma.word))
        .ensure("failed to insert grammeme");

    // followed by its relations to the grammemes set
    let mut stmt = conn
        .prepare_cached("INSERT INTO oc_lemma_grammemes (lemma, grammeme) VALUES (?1, ?2)")
        .ensure("failed to prepare statement");

    for grammeme in lemma.grammemes {
        stmt.execute((&lemma.id, grammeme))
            .ensure("failed to insert grammeme<>lemma relationship");
    }

    // followed by all of its variations ...
    let mut word_insert = conn
        .prepare_cached("INSERT INTO oc_words (lemma, word) VALUES (?1, ?2)")
        .unwrap();

    let mut word_grammeme = conn
        .prepare_cached("INSERT INTO oc_word_grammemes (word, grammeme) VALUES (?1, ?2)")
        .unwrap();

    for variation in lemma.variations {
        // insert the word itself and get its rowid
        word_insert
            .execute((&lemma.id, &variation.word))
            .ensure("failed to insert word");
        let row_id = conn.last_insert_rowid();

        // then insert its grammeme links
        for grammeme in variation.grammemes {
            word_grammeme
                .execute((row_id, grammeme))
                .ensure("failed to insert word<>grammeme link");
        }
    }

    debug!("inserted lemma {}", lemma.id);
}

/// Sets up an initial schema for the OpenRussian data.
pub fn initial_or_schema(conn: &Connection) {
    conn.execute_batch(
        r#"
CREATE TABLE or_words (
    id INTEGER PRIMARY KEY,
    bare TEXT NOT NULL,
    accented TEXT,
    derived_from_word_id INTEGER,
    rank TEXT,
    word_type TEXT,
    level TEXT
) STRICT;

CREATE TABLE or_words_forms (
    id INTEGER PRIMARY KEY,
    word_id INTEGER NOT NULL,
    form_type TEXT,
    position TEXT,
    form TEXT,
    form_bare TEXT,
    FOREIGN KEY(word_id) REFERENCES words(id)
) STRICT;

CREATE TABLE or_translations (
    id INTEGER PRIMARY KEY,
    word_id INTEGER NOT NULL,
    translation TEXT,
    example_ru TEXT,
    example_tl TEXT,
    info TEXT,
    FOREIGN KEY(word_id) REFERENCES words(id)
) STRICT;
"#,
    )
    .ensure("setting up OpenRussian table schema failed");

    info!("set up initial table schema for OpenRussian import");
}

pub fn insert_or_words<I: Iterator<Item = or_parser::Word>>(conn: &Connection, words: I) {
    let mut stmt = conn
        .prepare_cached(
            "
INSERT INTO or_words (id, bare, accented, derived_from_word_id, rank, word_type, level)
VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)
",
        )
        .ensure("failed to prepare OR words statement");
    let mut count = 0;

    for word in words {
        stmt.execute((
            word.id,
            word.bare,
            word.accented,
            word.derived_from_word_id,
            word.rank,
            word.word_type,
            word.level,
        ))
        .ensure("failed to insert OR word");
        count += 1;
    }

    info!("inserted {} OpenRussian words", count);
}

pub fn insert_or_word_forms<I: Iterator<Item = or_parser::WordForm>>(conn: &Connection, forms: I) {
    let mut stmt = conn
        .prepare_cached(
            "
INSERT INTO or_words_forms (id, word_id, form_type, position, form, form_bare)
VALUES (?1, ?2, ?3, ?4, ?5, ?6)
",
        )
        .ensure("failed to prepare OR word forms statement");
    let mut count = 0;

    for form in forms {
        stmt.execute((
            form.id,
            form.word_id,
            form.form_type,
            form.position,
            form.form,
            form.form_bare,
        ))
        .ensure("failed to insert OR word form");
        count += 1;
    }

    info!("inserted {} OpenRussian word forms", count);
}

pub fn insert_or_translations<I: Iterator<Item = or_parser::Translation>>(
    conn: &Connection,
    translations: I,
) {
    let mut stmt = conn
        .prepare_cached(
            "INSERT INTO or_translations (id, word_id, translation, example_ru, example_tl, info)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
        )
        .ensure("failed to prepare OR translation statement");

    let mut count = 0;

    for tl in translations {
        if tl.lang != "en" {
            continue;
        }

        stmt.execute((
            tl.id,
            tl.word_id,
            tl.tl,
            tl.example_ru,
            tl.example_tl,
            tl.info,
        ))
        .ensure("failed to insert OR translation");

        count += 1;
    }

    info!("inserted {} OpenRussian translations", count);
}
