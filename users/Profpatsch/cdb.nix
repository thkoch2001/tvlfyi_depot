{ depot, pkgs, ... }:

let
  cdbListToNetencode = depot.nix.writers.rustSimple
    {
      name = "cdb-list-to-netencode";
      dependencies = [
        depot.third_party.rust-crates.nom
        depot.users.Profpatsch.execline.exec-helpers
        depot.users.Profpatsch.netencode.netencode-rs
      ];
    } ''
    extern crate nom;
    extern crate exec_helpers;
    extern crate netencode;
    use std::collections::HashMap;
    use std::io::BufRead;
    use nom::{IResult};
    use nom::sequence::{tuple};
    use nom::bytes::complete::{tag, take};
    use nom::character::complete::{digit1, char};
    use nom::error::{context, ErrorKind, ParseError};
    use nom::combinator::{map_res};
    use netencode::{T, Tag};

    fn usize_t(s: &[u8]) -> IResult<&[u8], usize> {
        context(
            "usize",
            map_res(
                map_res(digit1, |n| std::str::from_utf8(n)),
                |s| s.parse::<usize>())
        )(s)
    }

    fn parse_cdb_record(s: &[u8]) -> IResult<&[u8], (&[u8], &[u8])> {
        let (s, (_, klen, _, vlen, _)) = tuple((
            char('+'),
            usize_t,
            char(','),
            usize_t,
            char(':')
        ))(s)?;
        let (s, (key, _, val)) = tuple((
            take(klen),
            tag("->"),
            take(vlen),
        ))(s)?;
        Ok((s, (key, val)))
    }

    fn main() {
        let mut res = vec![];
        let stdin = std::io::stdin();
        let mut lines = stdin.lock().split(b'\n');
        loop {
            match lines.next() {
                None => exec_helpers::die_user_error("cdb-list-to-netencode", "stdin ended but we didn’t receive the empty line to signify the end of the cdbdump input!"),
                Some(Err(err)) => exec_helpers::die_temporary("cdb-list-to-netencode", format!("could not read from stdin: {}", err)),
                Some(Ok(line)) =>
                    if &line == b"" {
                        // the cdbdump input ends after an empty line (double \n)
                        break;
                    } else {
                        match parse_cdb_record(&line) {
                            Ok((b"", (key, val))) => {
                                let (key, val) = match
                                    std::str::from_utf8(key)
                                    .and_then(|k| std::str::from_utf8(val).map(|v| (k, v))) {
                                    Ok((key, val)) => (key.to_owned(), val.to_owned()),
                                    Err(err) => exec_helpers::die_user_error("cdb-list-to-netencode", format!("cannot decode line {:?}, we only support utf8-encoded key/values pairs for now: {}", String::from_utf8_lossy(&line), err)),
                                };
                                let _ = res.push((key, val));
                            },
                            Ok((rest, _)) => exec_helpers::die_user_error("cdb-list-to-netencode", format!("could not decode record line {:?}, had some trailing bytes", String::from_utf8_lossy(&line))),
                            Err(err) => exec_helpers::die_user_error("cdb-list-to-netencode", format!("could not decode record line {:?}: {:?}", String::from_utf8_lossy(&line), err)),
                        }
                    }
            }
        }
        let list = T::List(res.into_iter().map(
            |(k, v)| T::Record(vec![(String::from("key"), T::Text(k)), (String::from("val"), T::Text(v))].into_iter().collect())
        ).collect());
        netencode::encode(&mut std::io::stdout(), &list.to_u());
    }

  '';

in
{
  inherit
    cdbListToNetencode
    ;
}
