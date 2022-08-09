use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

// From the serde_json docs:
//
// > There are three common ways that you might find yourself needing to work
// > with JSON data in Rust.
// >
// > 1. As text data. An unprocessed string of JSON data that you receive on an
// >    HTTP endpoint, read from a file, or prepare to send to a remote server.
// > 2. As an untyped or loosely typed representation. Maybe you want to check
// >    that some JSON data is valid before passing it on, but without knowing
// >    the structure of what it contains. Or you want to do very basic
// >    manipulations like insert a key in a particular spot.
// > 3. As a strongly typed Rust data structure. When you expect all or most of
// >    your data to conform to a particular structure and want to get real work
// >    done without JSON’s loosey-goosey nature tripping you up.
//
// So let's take a look at all three...

////////////////////////////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////////////////////////////

#[derive(Serialize, Deserialize, Debug)]
struct Person {
    fname: String,
    lname: String,
    age: u8,
}

////////////////////////////////////////////////////////////////////////////////
// Functions
////////////////////////////////////////////////////////////////////////////////

// 1) Reading/writing from/to plain text.
//    TL;DR:
//    - read:  serde_json::from_str(data)
//    - write: x.to_string()
pub fn one() {
    let data = json!({
        "fname": "William",
        "lname": "Carroll",
        "age": 30,
    })
    .to_string();

    println!("result: {:?}", data);
}

// 2) Parse into a loosely typed representation; mutate it; serialize it back.
//    TL;DR:
//    - read:  serde_json::from_str(data)
//    - write: x.to_string()
pub fn two() {
    let data = r#"{"fname":"William","lname":"Carroll","age":30}"#;

    let mut parsed: Value = serde_json::from_str(data).unwrap();
    parsed["fname"] = json!("Norm");
    parsed["lname"] = json!("Macdonald");
    parsed["age"] = json!(61);

    let result = parsed.to_string();
    println!("result: {:?}", result);
}

// 3) Parse into a strongly typed structure.
//    TL;DR:
//    - read:  serde_json::from_str(data)
//    - write: serde_json::to_string(x).unwrap()
pub fn three() {
    let data = r#"{"fname":"William","lname":"Carroll","age":30}"#;

    let mut read: Person = serde_json::from_str(data).unwrap();
    read.fname = "Norm".to_string();
    read.lname = "Macdonald".to_string();
    read.age = 61;

    let write = serde_json::to_string(&read).unwrap();
    println!("result: {:?}", write);
}
