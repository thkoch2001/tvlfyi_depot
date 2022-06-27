use serde_json::json;

// From the serde_json docs:
//
// > There are three common ways that you might find yourself needing to work with
// > JSON data in Rust.
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

// 1) Reading/writing from/to plain text.
//    TL;DR:
//    - read:  serde_json::from_str(data)
//    - write: x.to_string()
fn one() {
    let data = json!({
        "fname": "William",
        "lname": "Carroll",
        "age": 30,
    })
    .to_string();

    println!("result: {:?}", data);
}

fn main() {
    one()
}
