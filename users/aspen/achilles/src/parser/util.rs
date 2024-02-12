use nom::character::complete::multispace0;
use nom::{complete, map, named, tag, tuple};

named!(pub(crate) comma(&str) -> (), map!(tuple!(
    multispace0,
    complete!(tag!(",")),
    multispace0
) ,|_| ()));
