// Playing around with Rust's "smart pointers". Starting off with a wrapper type
// that allows multiple readers (owners?) of some data.

use std::rc::Rc;

pub fn example() {
    let five = Rc::new(5);
    let x = Rc::clone(&five);
    let y = Rc::clone(&five);
    let z = Rc::clone(&five);
    println!("result: {}", *x + *y + *z)
}
