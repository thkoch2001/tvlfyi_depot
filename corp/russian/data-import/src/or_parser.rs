//! Parser for the OpenRussian data format.
//!
//! Note that when exporting OpenRussian data from the project you
//! have to choose an encoding. We choose tab-separated CSV files, as
//! tabs have a very low probability of actually appearing in the
//! input data and this skips some potential encoding issues.

use super::Ensure;
use serde::Deserialize;
use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

/// A word from the `words` table.
#[derive(Debug, Deserialize)]
pub struct Word {
    pub id: usize,
    pub position: String, // TODO: unknown
    pub bare: String,     // TODO: unknown
    pub accented: String, // TODO: unknown
    pub derived_from_word_id: Option<usize>,
    pub rank: String,         // TODO: unknown
    pub disabled: String,     // TODO: unknown
    pub audio: String,        // TODO: unknown
    pub usage_en: String,     // TODO: unknown
    pub usage_de: String,     // TODO: unknown
    pub number_value: String, // TODO: unknown

    #[serde(rename = "type")]
    pub word_type: String, // TODO: unknown

    pub level: String,      // TODO: unknown
    pub created_at: String, // TODO: unknown
}

pub struct OpenRussianParser {
    or_directory: PathBuf,
}

pub type DynIter<T> = Box<dyn Iterator<Item = T>>;

impl OpenRussianParser {
    pub fn new<P: Into<PathBuf>>(path: P) -> Self {
        OpenRussianParser {
            or_directory: path.into(),
        }
    }

    pub fn words(&self) -> DynIter<Word> {
        self.parser_for("words.csv")
    }

    fn parser_for<T: serde::de::DeserializeOwned + 'static>(
        &self,
        file_name: &str,
    ) -> Box<dyn Iterator<Item = T>> {
        let mut path = self.or_directory.clone();
        path.push(file_name);

        let reader = csv::ReaderBuilder::new()
            .delimiter(b'\t')
            .from_reader(BufReader::new(
                File::open(&path).ensure("failed to open words.csv"),
            ));

        Box::new(reader.into_deserialize().map(|result| {
            result.ensure(format!(
                "failed to deserialize {}",
                std::any::type_name::<T>()
            ))
        }))
    }
}
