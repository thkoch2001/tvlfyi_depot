//! Tvix's bindings to the Memory Management Toolkit API.

use mmtk::util::{Address, ObjectReference, VMWorkerThread};
use mmtk::vm::edge_shape::SimpleEdge;
use mmtk::vm::*;
use mmtk::Mutator;

mod active_plan;
mod collection;
mod object_model;
mod reference_glue;
mod scanning;

/// Carrier type for the traits required by MMTk.
#[derive(Default)]
struct TvixEval;

impl VMBinding for TvixEval {
    type VMObjectModel = object_model::TvixObjectModel;
    type VMScanning = scanning::TvixScanning;
    type VMEdge = SimpleEdge;
    type VMCollection = collection::TvixCollection;
    type VMActivePlan = active_plan::TvixActivePlan;
    type VMReferenceGlue = reference_glue::TvixReferenceGlue;
    type VMMemorySlice = edge_shape::UnimplementedMemorySlice;
}
