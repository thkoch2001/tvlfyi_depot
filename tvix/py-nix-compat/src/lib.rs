use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
};

use nix_compat::derivation::{Derivation, Output};
use pyo3::prelude::*;

#[pyclass]
struct PyOutput {
    #[pyo3(get, set)]
    pub path: Option<String>,
    #[pyo3(get, set)]
    pub ca_hash: Option<String>,
}

impl From<Output> for PyOutput {
    fn from(value: Output) -> Self {
        Self {
            path: value.path.map(|p| p.to_absolute_path()),
            ca_hash: value
                .ca_hash
                .map(|ca_hash| ca_hash.to_nix_nixbase32_string()),
        }
    }
}

#[pyclass]
struct PyDerivation {
    #[pyo3(get, set)]
    pub arguments: Vec<String>,
    #[pyo3(get, set)]
    pub builder: String,
    #[pyo3(set)]
    pub environment: BTreeMap<String, Vec<u8>>,
    #[pyo3(get, set)]
    pub input_derivations: BTreeMap<String, BTreeSet<String>>,
    #[pyo3(get, set)]
    pub input_sources: BTreeSet<String>,
    #[pyo3(get, set)]
    pub outputs: BTreeMap<String, (Option<String>, Option<String>)>,
    #[pyo3(get, set)]
    pub system: String,
}

impl From<Derivation> for PyDerivation {
    fn from(value: Derivation) -> Self {
        PyDerivation {
            arguments: value.arguments,
            builder: value.builder,
            environment: value
                .environment
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            input_derivations: value
                .input_derivations
                .into_iter()
                .map(|(k, v)| (k.to_absolute_path(), v))
                .collect(),
            input_sources: value
                .input_sources
                .into_iter()
                .map(|v| v.to_absolute_path())
                .collect(),
            outputs: value
                .outputs
                .into_iter()
                .map(|(k, v)| {
                    let v: PyOutput = v.into();
                    (k, (v.path, v.ca_hash))
                })
                .collect(),
            system: value.system,
        }
    }
}

#[pymethods]
impl PyDerivation {
    #[staticmethod]
    pub fn from_aterm_bytes(b: &[u8]) -> PyResult<Self> {
        Derivation::from_aterm_bytes(b)
            .map(|d| d.into())
            .map_err(|err| PyErr::new::<Self, String>(format!("{:#?}", err)))
    }

    #[getter]
    pub fn environment(&self) -> PyResult<BTreeMap<String, Cow<[u8]>>> {
        Ok(self
            .environment
            .clone()
            .into_iter()
            .map(|(k, v)| (k, v.into()))
            .collect())
    }
}

/// A Python module implemented in Rust.
#[pymodule]
fn py_nix_compat(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<PyDerivation>()?;
    Ok(())
}
