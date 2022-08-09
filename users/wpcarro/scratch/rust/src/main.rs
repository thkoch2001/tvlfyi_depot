use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

mod display;
mod json;

////////////////////////////////////////////////////////////////////////////////
// Main
////////////////////////////////////////////////////////////////////////////////

fn main() {
    let john: display::Person = display::Person {
        fname: "John".to_string(),
        lname: "Cleese".to_string(),
        age: 82,
    };
    println!("Person: {}", john)
}
