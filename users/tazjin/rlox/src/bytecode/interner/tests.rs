use super::*;

#[test]
fn interns_strings() {
    let mut interner = Interner::with_capacity(128);
    let id = interner.intern("hello world");
    assert_eq!("hello world", interner.lookup(id));
}

#[test]
fn deduplicates_strings() {
    let mut interner = Interner::with_capacity(128);
    let id_1 = interner.intern("hello world");
    let id_2 = interner.intern("hello world");
    assert_eq!(id_1, id_2);
}

#[test]
fn ids_survive_growing() {
    let mut interner = Interner::with_capacity(16);
    let id = interner.intern("hello");
    interner.intern("excessively large string that will cause eallocation");
    assert_eq!("hello", interner.lookup(id));
}
