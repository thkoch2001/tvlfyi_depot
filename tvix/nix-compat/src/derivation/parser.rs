//! This module constructs a [Derivation] by parsing its [ATerm][]
//! serialization.
//!
//! [ATerm]: http://program-transformation.org/Tools/ATermFormat.html

use nom::bytes::complete::tag;
use nom::character::complete::char as nomchar;
use nom::combinator::{all_consuming, map_res};
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, preceded, separated_pair, terminated, tuple};
use std::collections::{btree_map, BTreeMap, BTreeSet};
use thiserror;

use crate::derivation::parse_error::{into_nomerror, ErrorKind, NomError, NomResult};
use crate::derivation::{write, CAHash, Derivation, Output};
use crate::store_path::{self, StorePath};
use crate::{aterm, nixhash};

#[derive(Debug, thiserror::Error)]
pub enum Error<I> {
    #[error("parsing error: {0}")]
    Parser(NomError<I>),
    #[error("premature EOF")]
    Incomplete,
    #[error("validation error: {0}")]
    Validation(super::DerivationError),
}

pub(crate) fn parse(i: &[u8]) -> Result<Derivation, Error<&[u8]>> {
    match all_consuming(parse_derivation)(i) {
        Ok((rest, derivation)) => {
            // this shouldn't happen, as all_consuming shouldn't return.
            debug_assert!(rest.is_empty());

            // invoke validate
            derivation.validate(true).map_err(Error::Validation)?;

            Ok(derivation)
        }
        Err(nom::Err::Incomplete(_)) => Err(Error::Incomplete),
        Err(nom::Err::Error(e) | nom::Err::Failure(e)) => Err(Error::Parser(e)),
    }
}

/// Consume a string containing the algo, and optionally a `r:`
/// prefix, and a digest (bytes), return a [CAHash::Nar] or [CAHash::Flat].
fn from_algo_and_mode_and_digest<B: AsRef<[u8]>>(
    algo_and_mode: &str,
    digest: B,
) -> crate::nixhash::NixHashResult<CAHash> {
    Ok(match algo_and_mode.strip_prefix("r:") {
        Some(algo) => nixhash::CAHash::Nar(nixhash::from_algo_and_digest(
            algo.try_into()?,
            digest.as_ref(),
        )?),
        None => nixhash::CAHash::Flat(nixhash::from_algo_and_digest(
            algo_and_mode.try_into()?,
            digest.as_ref(),
        )?),
    })
}

/// Parse one output in ATerm. This is 4 string fields inside parans:
/// output name, output path, algo (and mode), digest.
/// Returns the output name and [Output] struct.
fn parse_output(i: &[u8]) -> NomResult<&[u8], (String, Output)> {
    delimited(
        nomchar('('),
        map_res(
            |i| {
                tuple((
                    terminated(aterm::parse_string_field, nomchar(',')),
                    terminated(aterm::parse_string_field, nomchar(',')),
                    terminated(aterm::parse_string_field, nomchar(',')),
                    aterm::parse_bytes_field,
                ))(i)
                .map_err(into_nomerror)
            },
            |(output_name, output_path, algo_and_mode, encoded_digest)| {
                // convert these 4 fields into an [Output].
                let ca_hash_res = {
                    if algo_and_mode.is_empty() && encoded_digest.is_empty() {
                        None
                    } else {
                        match data_encoding::HEXLOWER.decode(&encoded_digest) {
                            Ok(digest) => {
                                Some(from_algo_and_mode_and_digest(&algo_and_mode, digest))
                            }
                            Err(e) => Some(Err(nixhash::Error::InvalidBase64Encoding(e))),
                        }
                    }
                }
                .transpose();

                match ca_hash_res {
                    Ok(hash_with_mode) => Ok((
                        output_name,
                        Output {
                            // TODO: Check if allowing empty paths here actually makes sense
                            //       or we should make this code stricter.
                            path: if output_path.is_empty() {
                                None
                            } else {
                                Some(string_to_store_path(i, &output_path)?)
                            },
                            ca_hash: hash_with_mode,
                        },
                    )),
                    Err(e) => Err(nom::Err::Failure(NomError {
                        input: i,
                        code: ErrorKind::NixHashError(e),
                    })),
                }
            },
        ),
        nomchar(')'),
    )(i)
}

/// Parse multiple outputs in ATerm. This is a list of things acccepted by
/// parse_output, and takes care of turning the (String, Output) returned from
/// it to a BTreeMap.
/// We don't use parse_kv here, as it's dealing with 2-tuples, and these are
/// 4-tuples.
fn parse_outputs(i: &[u8]) -> NomResult<&[u8], BTreeMap<String, Output>> {
    let res = delimited(
        nomchar('['),
        separated_list1(tag(","), parse_output),
        nomchar(']'),
    )(i);

    match res {
        Ok((rst, outputs_lst)) => {
            let mut outputs = BTreeMap::default();
            for (output_name, output) in outputs_lst.into_iter() {
                if outputs.contains_key(&output_name) {
                    return Err(nom::Err::Failure(NomError {
                        input: i,
                        code: ErrorKind::DuplicateMapKey(output_name.to_string()),
                    }));
                }
                outputs.insert(output_name, output);
            }
            Ok((rst, outputs))
        }
        // pass regular parse errors along
        Err(e) => Err(e),
    }
}

fn parse_input_derivations(
    i: &[u8],
) -> NomResult<&[u8], BTreeMap<StorePath<String>, BTreeSet<String>>> {
    let (i, input_derivations_list) = parse_kv(aterm::parse_string_list)(i)?;

    // This is a HashMap of drv paths to a list of output names.
    let mut input_derivations: BTreeMap<StorePath<String>, BTreeSet<_>> = BTreeMap::new();

    for (input_derivation, output_names) in input_derivations_list {
        let mut new_output_names = BTreeSet::new();
        for output_name in output_names.into_iter() {
            if new_output_names.contains(&output_name) {
                return Err(nom::Err::Failure(NomError {
                    input: i,
                    code: ErrorKind::DuplicateInputDerivationOutputName(
                        input_derivation.to_string(),
                        output_name.to_string(),
                    ),
                }));
            }
            new_output_names.insert(output_name);
        }

        let input_derivation = string_to_store_path(i, input_derivation.as_str())?;

        input_derivations.insert(input_derivation, new_output_names);
    }

    Ok((i, input_derivations))
}

fn parse_input_sources(i: &[u8]) -> NomResult<&[u8], BTreeSet<StorePath<String>>> {
    let (i, input_sources_lst) = aterm::parse_string_list(i).map_err(into_nomerror)?;

    let mut input_sources: BTreeSet<_> = BTreeSet::new();
    for input_source in input_sources_lst.into_iter() {
        let input_source = string_to_store_path(i, input_source.as_str())?;
        if input_sources.contains(&input_source) {
            return Err(nom::Err::Failure(NomError {
                input: i,
                code: ErrorKind::DuplicateInputSource(input_source.to_owned()),
            }));
        } else {
            input_sources.insert(input_source);
        }
    }

    Ok((i, input_sources))
}

fn string_to_store_path<'a, 'i, S>(
    i: &'i [u8],
    path_str: &'a str,
) -> Result<StorePath<S>, nom::Err<NomError<&'i [u8]>>>
where
    S: std::clone::Clone + AsRef<str> + std::convert::From<&'a str>,
{
    let path =
        StorePath::from_absolute_path(path_str.as_bytes()).map_err(|e: store_path::Error| {
            nom::Err::Failure(NomError {
                input: i,
                code: e.into(),
            })
        })?;

    #[cfg(debug_assertions)]
    assert_eq!(path_str, path.to_absolute_path());

    Ok(path)
}

pub fn parse_derivation(i: &[u8]) -> NomResult<&[u8], Derivation> {
    use nom::Parser;
    preceded(
        tag(write::DERIVATION_PREFIX),
        delimited(
            // inside parens
            nomchar('('),
            // tuple requires all errors to be of the same type, so we need to be a
            // bit verbose here wrapping generic IResult into [NomATermResult].
            tuple((
                // parse outputs
                terminated(parse_outputs, nomchar(',')),
                // // parse input derivations
                terminated(parse_input_derivations, nomchar(',')),
                // // parse input sources
                terminated(parse_input_sources, nomchar(',')),
                // // parse system
                |i| terminated(aterm::parse_string_field, nomchar(','))(i).map_err(into_nomerror),
                // // parse builder
                |i| terminated(aterm::parse_string_field, nomchar(','))(i).map_err(into_nomerror),
                // // parse arguments
                |i| terminated(aterm::parse_string_list, nomchar(','))(i).map_err(into_nomerror),
                // parse environment
                parse_kv(aterm::parse_bytes_field),
            )),
            nomchar(')'),
        )
        .map(
            |(
                outputs,
                input_derivations,
                input_sources,
                system,
                builder,
                arguments,
                environment,
            )| {
                Derivation {
                    arguments,
                    builder,
                    environment,
                    input_derivations,
                    input_sources,
                    outputs,
                    system,
                }
            },
        ),
    )(i)
}

/// Parse a list of key/value pairs into a BTreeMap.
/// The parser for the values can be passed in.
/// In terms of ATerm, this is just a 2-tuple,
/// but we have the additional restriction that the first element needs to be
/// unique across all tuples.
pub(crate) fn parse_kv<'a, V, VF>(
    vf: VF,
) -> impl FnMut(&'a [u8]) -> NomResult<&'a [u8], BTreeMap<String, V>> + 'static
where
    VF: FnMut(&'a [u8]) -> nom::IResult<&'a [u8], V, nom::error::Error<&'a [u8]>> + Clone + 'static,
{
    move |i|
    // inside brackets
    delimited(
        nomchar('['),
        |ii| {
            let res = separated_list0(
                nomchar(','),
                // inside parens
                delimited(
                    nomchar('('),
                    separated_pair(
                        aterm::parse_string_field,
                        nomchar(','),
                        vf.clone(),
                    ),
                    nomchar(')'),
                ),
            )(ii).map_err(into_nomerror);

            match res {
                Ok((rest, pairs)) => {
                    let mut kvs: BTreeMap<String, V> = BTreeMap::new();
                    for (k, v) in pairs.into_iter() {
                        // collect the 2-tuple to a BTreeMap,
                        // and fail if the key was already seen before.
                        match kvs.entry(k) {
                            btree_map::Entry::Vacant(e) => { e.insert(v); },
                            btree_map::Entry::Occupied(e) => {
                                return Err(nom::Err::Failure(NomError {
                                    input: i,
                                    code: ErrorKind::DuplicateMapKey(e.key().clone()),
                                }));
                            }
                        }
                    }
                    Ok((rest, kvs))
                }
                Err(e) => Err(e),
            }
        },
        nomchar(']'),
    )(i)
}

#[cfg(test)]
mod tests {
    use crate::store_path::StorePathRef;
    use std::collections::{BTreeMap, BTreeSet};
    use std::sync::LazyLock;

    use crate::{
        derivation::{
            parse_error::ErrorKind, parser::from_algo_and_mode_and_digest, CAHash, NixHash, Output,
        },
        store_path::StorePath,
    };
    use bstr::{BString, ByteSlice};
    use hex_literal::hex;
    use rstest::rstest;

    const DIGEST_SHA256: [u8; 32] =
        hex!("a5ce9c155ed09397614646c9717fc7cd94b1023d7b76b618d409e4fefd6e9d39");

    static NIXHASH_SHA256: NixHash = NixHash::Sha256(DIGEST_SHA256);
    static EXP_MULTI_OUTPUTS: LazyLock<BTreeMap<String, Output>> = LazyLock::new(|| {
        let mut b = BTreeMap::new();
        b.insert(
            "lib".to_string(),
            Output {
                path: Some(
                    StorePath::from_bytes(b"2vixb94v0hy2xc6p7mbnxxcyc095yyia-has-multi-out-lib")
                        .unwrap(),
                ),
                ca_hash: None,
            },
        );
        b.insert(
            "out".to_string(),
            Output {
                path: Some(
                    StorePath::from_bytes(
                        b"55lwldka5nyxa08wnvlizyqw02ihy8ic-has-multi-out".as_bytes(),
                    )
                    .unwrap(),
                ),
                ca_hash: None,
            },
        );
        b
    });

    static EXP_AB_MAP: LazyLock<BTreeMap<String, BString>> = LazyLock::new(|| {
        let mut b = BTreeMap::new();
        b.insert("a".to_string(), b"1".into());
        b.insert("b".to_string(), b"2".into());
        b
    });

    static EXP_INPUT_DERIVATIONS_SIMPLE: LazyLock<BTreeMap<StorePath<String>, BTreeSet<String>>> =
        LazyLock::new(|| {
            let mut b = BTreeMap::new();
            b.insert(
                StorePath::from_bytes(b"8bjm87p310sb7r2r0sg4xrynlvg86j8k-hello-2.12.1.tar.gz.drv")
                    .unwrap(),
                {
                    let mut output_names = BTreeSet::new();
                    output_names.insert("out".to_string());
                    output_names
                },
            );
            b.insert(
                StorePath::from_bytes(b"p3jc8aw45dza6h52v81j7lk69khckmcj-bash-5.2-p15.drv")
                    .unwrap(),
                {
                    let mut output_names = BTreeSet::new();
                    output_names.insert("out".to_string());
                    output_names.insert("lib".to_string());
                    output_names
                },
            );
            b
        });

    static EXP_INPUT_DERIVATIONS_SIMPLE_ATERM: LazyLock<String> = LazyLock::new(|| {
        format!(
            "[(\"{0}\",[\"out\"]),(\"{1}\",[\"out\",\"lib\"])]",
            "/nix/store/8bjm87p310sb7r2r0sg4xrynlvg86j8k-hello-2.12.1.tar.gz.drv",
            "/nix/store/p3jc8aw45dza6h52v81j7lk69khckmcj-bash-5.2-p15.drv"
        )
    });

    static EXP_INPUT_SOURCES_SIMPLE: LazyLock<BTreeSet<String>> = LazyLock::new(|| {
        let mut b = BTreeSet::new();
        b.insert("/nix/store/55lwldka5nyxa08wnvlizyqw02ihy8ic-has-multi-out".to_string());
        b.insert("/nix/store/2vixb94v0hy2xc6p7mbnxxcyc095yyia-has-multi-out-lib".to_string());
        b
    });

    /// Ensure parsing KVs works
    #[rstest]
    #[case::empty(b"[]", &BTreeMap::new(), b"")]
    #[case::simple(b"[(\"a\",\"1\"),(\"b\",\"2\")]", &EXP_AB_MAP, b"")]
    fn parse_kv(
        #[case] input: &'static [u8],
        #[case] expected: &BTreeMap<String, BString>,
        #[case] exp_rest: &[u8],
    ) {
        let (rest, parsed) =
            super::parse_kv(crate::aterm::parse_bytes_field)(input).expect("must parse");
        assert_eq!(exp_rest, rest, "expected remainder");
        assert_eq!(*expected, parsed);
    }

    /// Ensures the kv parser complains about duplicate map keys
    #[test]
    fn parse_kv_fail_dup_keys() {
        let input: &'static [u8] = b"[(\"a\",\"1\"),(\"a\",\"2\")]";
        let e = super::parse_kv(crate::aterm::parse_bytes_field)(input).expect_err("must fail");

        match e {
            nom::Err::Failure(e) => {
                assert_eq!(ErrorKind::DuplicateMapKey("a".to_string()), e.code);
            }
            _ => panic!("unexpected error"),
        }
    }

    /// Ensure parsing input derivations works.
    #[rstest]
    #[case::empty(b"[]", &BTreeMap::new())]
    #[case::simple(EXP_INPUT_DERIVATIONS_SIMPLE_ATERM.as_bytes(), &EXP_INPUT_DERIVATIONS_SIMPLE)]
    fn parse_input_derivations(
        #[case] input: &'static [u8],
        #[case] expected: &BTreeMap<StorePath<String>, BTreeSet<String>>,
    ) {
        let (rest, parsed) = super::parse_input_derivations(input).expect("must parse");

        assert_eq!(expected, &parsed, "parsed mismatch");
        assert!(rest.is_empty(), "rest must be empty");
    }

    /// Ensures the input derivation parser complains about duplicate output names
    #[test]
    fn parse_input_derivations_fail_dup_output_names() {
        let input_str = format!(
            "[(\"{0}\",[\"out\"]),(\"{1}\",[\"out\",\"out\"])]",
            "/nix/store/8bjm87p310sb7r2r0sg4xrynlvg86j8k-hello-2.12.1.tar.gz.drv",
            "/nix/store/p3jc8aw45dza6h52v81j7lk69khckmcj-bash-5.2-p15.drv"
        );
        let e = super::parse_input_derivations(input_str.as_bytes()).expect_err("must fail");

        match e {
            nom::Err::Failure(e) => {
                assert_eq!(
                    ErrorKind::DuplicateInputDerivationOutputName(
                        "/nix/store/p3jc8aw45dza6h52v81j7lk69khckmcj-bash-5.2-p15.drv".to_string(),
                        "out".to_string()
                    ),
                    e.code
                );
            }
            _ => panic!("unexpected error"),
        }
    }

    /// Ensure parsing input sources works
    #[rstest]
    #[case::empty(b"[]", &BTreeSet::new())]
    #[case::simple(b"[\"/nix/store/55lwldka5nyxa08wnvlizyqw02ihy8ic-has-multi-out\",\"/nix/store/2vixb94v0hy2xc6p7mbnxxcyc095yyia-has-multi-out-lib\"]", &EXP_INPUT_SOURCES_SIMPLE)]
    fn parse_input_sources(#[case] input: &'static [u8], #[case] expected: &BTreeSet<String>) {
        let (rest, parsed) = super::parse_input_sources(input).expect("must parse");

        assert_eq!(
            expected,
            &parsed
                .iter()
                .map(StorePath::to_absolute_path)
                .collect::<BTreeSet<_>>(),
            "parsed mismatch"
        );
        assert!(rest.is_empty(), "rest must be empty");
    }

    /// Ensures the input sources parser complains about duplicate input sources
    #[test]
    fn parse_input_sources_fail_dup_keys() {
        let input: &'static [u8] = b"[\"/nix/store/55lwldka5nyxa08wnvlizyqw02ihy8ic-foo\",\"/nix/store/55lwldka5nyxa08wnvlizyqw02ihy8ic-foo\"]";
        let e = super::parse_input_sources(input).expect_err("must fail");

        match e {
            nom::Err::Failure(e) => {
                assert_eq!(
                    ErrorKind::DuplicateInputSource(
                        StorePathRef::from_absolute_path(
                            "/nix/store/55lwldka5nyxa08wnvlizyqw02ihy8ic-foo".as_bytes()
                        )
                        .unwrap()
                        .to_owned()
                    ),
                    e.code
                );
            }
            _ => panic!("unexpected error"),
        }
    }

    #[rstest]
    #[case::simple(
        br#"("out","/nix/store/5vyvcwah9l9kf07d52rcgdk70g2f4y13-foo","","")"#,
        ("out".to_string(), Output {
            path: Some(
                StorePathRef::from_absolute_path("/nix/store/5vyvcwah9l9kf07d52rcgdk70g2f4y13-foo".as_bytes()).unwrap().to_owned()),
            ca_hash: None
        })
    )]
    #[case::fod(
        br#"("out","/nix/store/4q0pg5zpfmznxscq3avycvf9xdvx50n3-bar","r:sha256","08813cbee9903c62be4c5027726a418a300da4500b2d369d3af9286f4815ceba")"#,
        ("out".to_string(), Output {
            path: Some(
                StorePathRef::from_absolute_path(
                "/nix/store/4q0pg5zpfmznxscq3avycvf9xdvx50n3-bar".as_bytes()).unwrap().to_owned()),
            ca_hash: Some(from_algo_and_mode_and_digest("r:sha256",
                   data_encoding::HEXLOWER.decode(b"08813cbee9903c62be4c5027726a418a300da4500b2d369d3af9286f4815ceba").unwrap()            ).unwrap()),
        })
     )]
    fn parse_output(#[case] input: &[u8], #[case] expected: (String, Output)) {
        let (rest, parsed) = super::parse_output(input).expect("must parse");
        assert!(rest.is_empty());
        assert_eq!(expected, parsed);
    }

    #[rstest]
    #[case::multi_out(
        br#"[("lib","/nix/store/2vixb94v0hy2xc6p7mbnxxcyc095yyia-has-multi-out-lib","",""),("out","/nix/store/55lwldka5nyxa08wnvlizyqw02ihy8ic-has-multi-out","","")]"#,
        &EXP_MULTI_OUTPUTS
    )]
    fn parse_outputs(#[case] input: &[u8], #[case] expected: &BTreeMap<String, Output>) {
        let (rest, parsed) = super::parse_outputs(input).expect("must parse");
        assert!(rest.is_empty());
        assert_eq!(*expected, parsed);
    }

    #[rstest]
    #[case::sha256_flat("sha256", &DIGEST_SHA256, CAHash::Flat(NIXHASH_SHA256.clone()))]
    #[case::sha256_recursive("r:sha256", &DIGEST_SHA256, CAHash::Nar(NIXHASH_SHA256.clone()))]
    fn test_from_algo_and_mode_and_digest(
        #[case] algo_and_mode: &str,
        #[case] digest: &[u8],
        #[case] expected: CAHash,
    ) {
        assert_eq!(
            expected,
            from_algo_and_mode_and_digest(algo_and_mode, digest).unwrap()
        );
    }

    #[test]
    fn from_algo_and_mode_and_digest_failure() {
        assert!(from_algo_and_mode_and_digest("r:sha256", []).is_err());
        assert!(from_algo_and_mode_and_digest("ha256", DIGEST_SHA256).is_err());
    }
}
