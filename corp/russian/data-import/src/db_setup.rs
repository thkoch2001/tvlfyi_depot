//! This module prepares the database layout.
//!
//! The XML import may be in an arbitrary order, so importing data is
//! a multi-step process where we first set up schemas matching the
//! data layout, import the data, and then modify the schema to
//! introduce things like foreign key constraints between tables that
//! represent relations.

use super::{bail, Ensure};
use crate::oc_parser::*;
use log::{debug, info};
use rusqlite::Connection;

/// Sets up an initial schema which matches the OpenCorpora data.
pub fn initial_schema(conn: &Connection) {
    conn.execute_batch(
        r#"
-- table for plain import of grammemes from XML
CREATE TABLE grammemes (
    name TEXT PRIMARY KEY,
    parent TEXT,
    alias TEXT,
    description TEXT
) STRICT;

-- table for plain import of lemmas (*not* their variations!)
CREATE TABLE lemmas (
    id INTEGER PRIMARY KEY,
    lemma TEXT NOT NULL
) STRICT;

-- table for relationship between grammemes and lemmas
CREATE TABLE lemma_grammemes (
    lemma INTEGER,
    grammeme TEXT NOT NULL,
    FOREIGN KEY(lemma) REFERENCES lemmas(id)
) STRICT;

-- table for all words, i.e. including variations of lemmata
CREATE TABLE words (
    lemma INTEGER NOT NULL,
    word TEXT NOT NULL,
    FOREIGN KEY(lemma) REFERENCES lemmas(id)
) STRICT;

-- table for relationship between words and grammemes
CREATE TABLE word_grammemes (
    word INTEGER NOT NULL,
    grammeme TEXT NOT NULL,
    FOREIGN KEY(word) REFERENCES words(ROWID)
) STRICT;

-- table for link types
CREATE TABLE link_types (
  id INTEGER PRIMARY KEY,
  name TEXT
) STRICT;

"#,
    )
    .ensure("setting up initial table schema failed");

    info!("set up initial table schema for OpenCorpora import");
}

/// Inserts a single OpenCorpora element into the initial table structure.
pub fn insert_oc_element(conn: &Connection, elem: OcElement) {
    match elem {
        OcElement::Grammeme(grammeme) => {
            conn.execute(
                "INSERT INTO grammemes (name, parent, alias, description) VALUES (?1, ?2, ?3, ?4)",
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
                "INSERT INTO link_types (id, name) VALUES (?1, ?2)",
                (&lt.id, &lt.name),
            )
            .ensure("failed to insert link type");

            info!("inserted link type {}", lt.name);
        }
    }
}

/// Insert a single lemma into the initial structure. This is somewhat
/// involved because it also establishes a bunch of relations.
fn insert_lemma(conn: &Connection, lemma: Lemma) {
    // insert the lemma itself
    let mut stmt = conn
        .prepare_cached("INSERT INTO lemmas (id, lemma) VALUES (?1, ?2)")
        .ensure("failed to prepare statement");

    stmt.execute((&lemma.id, &lemma.lemma.word))
        .ensure("failed to insert grammeme");

    // followed by its relations to the grammemes set
    let mut stmt = conn
        .prepare_cached("INSERT INTO lemma_grammemes (lemma, grammeme) VALUES (?1, ?2)")
        .ensure("failed to prepare statement");

    for grammeme in lemma.grammemes {
        stmt.execute((&lemma.id, grammeme))
            .ensure("failed to insert grammeme<>lemma relationship");
    }

    // followed by all of its variations ...
    let mut word_insert = conn
        .prepare_cached("INSERT INTO words (lemma, word) VALUES (?1, ?2)")
        .unwrap();

    let mut word_grammeme = conn
        .prepare_cached("INSERT INTO word_grammemes (word, grammeme) VALUES (?1, ?2)")
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
