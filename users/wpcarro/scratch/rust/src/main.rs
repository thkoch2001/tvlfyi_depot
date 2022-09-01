use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

mod display;
mod json;
mod stdin;
mod rc;

////////////////////////////////////////////////////////////////////////////////
// Main
////////////////////////////////////////////////////////////////////////////////

fn main() {
    rc::example();
}
