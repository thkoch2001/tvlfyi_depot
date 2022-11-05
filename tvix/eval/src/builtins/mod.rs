//! This module implements the builtins exposed in the Nix language.
//!
//! See //tvix/eval/docs/builtins.md for a some context on the
//! available builtins in Nix.

use crate::compiler::{GlobalsMap, GlobalsMapFunc};
use crate::source::SourceCode;
use std::cmp::{self, Ordering};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::PathBuf;
use std::rc::Rc;

use regex::Regex;

use crate::warnings::WarningKind;
use crate::{
    errors::ErrorKind,
    value::{Builtin, CoercionKind, NixAttrs, NixList, NixString, Value},
    vm::VM,
};

use crate::arithmetic_op;

use self::versions::{VersionPart, VersionPartsIter};

#[cfg(feature = "impure")]
pub mod impure;
pub mod versions;

/// Coerce a Nix Value to a plain path, e.g. in order to access the
/// file it points to via either `builtins.toPath` or an impure
/// builtin. This coercion can _never_ be performed in a Nix program
/// without using builtins (i.e. the trick `path: /. + path` to
/// convert from a string to a path wouldn't hit this code).
pub fn coerce_value_to_path(v: &Value) -> Result<PathBuf, ErrorKind> {
    let value = v.force()?;
    match &*value {
        Value::Thunk(t) => coerce_value_to_path(&t.value()),
        Value::Path(p) => Ok(p.clone()),
        _ => value
            .coerce_to_string(CoercionKind::Weak)
            .map(|s| PathBuf::from(s.as_str()))
            .and_then(|path| {
                if path.is_absolute() {
                    Ok(path)
                } else {
                    Err(ErrorKind::NotAnAbsolutePath(path))
                }
            }),
    }
}

/// Return all pure builtins, that is all builtins that do not rely on
/// I/O outside of the VM and which can be used in any contexts (e.g.
/// WASM).
fn pure_builtins() -> Vec<Builtin> {
    vec![
        Builtin::new("abort", &[true], |args: Vec<Value>| {
            Err(ErrorKind::Abort(args[0].to_str()?.to_string()))
        }),
        Builtin::new(
            "add",
            &[false, false],
            |args: Vec<Value>| arithmetic_op!(&*args[0].force()?, &*args[1].force()?, +),
        ),
        Builtin::new("all", &[true, true], |args: Vec<Value>| {
            for value in args[1].to_list()?.into_iter() {
                let pred_result = VM::vm_call_with(&args[0], [value])?;

                if !pred_result.force()?.as_bool()? {
                    return Ok(Value::Bool(false));
                }
            }

            Ok(Value::Bool(true))
        }),
        Builtin::new("any", &[true, true], |args: Vec<Value>| {
            for value in args[1].to_list()?.into_iter() {
                let pred_result = VM::vm_call_with(&args[0], [value])?;

                if pred_result.force()?.as_bool()? {
                    return Ok(Value::Bool(true));
                }
            }

            Ok(Value::Bool(false))
        }),
        Builtin::new("attrNames", &[true], |args: Vec<Value>| {
            let xs = args[0].to_attrs()?;
            let mut output = Vec::with_capacity(xs.len());

            for (key, _val) in xs.iter() {
                output.push(Value::String(key.clone()));
            }

            Ok(Value::List(NixList::construct(output.len(), output)))
        }),
        Builtin::new("attrValues", &[true], |args: Vec<Value>| {
            let xs = args[0].to_attrs()?;
            let mut output = Vec::with_capacity(xs.len());

            for (_key, val) in xs.iter() {
                output.push(val.clone());
            }

            Ok(Value::List(NixList::construct(output.len(), output)))
        }),
        Builtin::new("baseNameOf", &[true], |args: Vec<Value>| {
            let s = args[0].coerce_to_string(CoercionKind::Weak)?;
            let result: String = s.rsplit_once('/').map(|(_, x)| x).unwrap_or(&s).into();
            Ok(result.into())
        }),
        Builtin::new("bitAnd", &[true, true], |args: Vec<Value>| {
            Ok(Value::Integer(args[0].as_int()? & args[1].as_int()?))
        }),
        Builtin::new("bitOr", &[true, true], |args: Vec<Value>| {
            Ok(Value::Integer(args[0].as_int()? | args[1].as_int()?))
        }),
        Builtin::new("bitXor", &[true, true], |args: Vec<Value>| {
            Ok(Value::Integer(args[0].as_int()? ^ args[1].as_int()?))
        }),
        Builtin::new("catAttrs", &[true, true], |args: Vec<Value>| {
            let key = args[0].to_str()?;
            let list = args[1].to_list()?;
            let mut output = vec![];

            for item in list.into_iter() {
                let set = item.force()?.to_attrs()?;
                if let Some(value) = set.select(key.as_str()) {
                    output.push(value.clone());
                }
            }

            Ok(Value::List(NixList::construct(output.len(), output)))
        }),
        Builtin::new("ceil", &[true], |args: Vec<Value>| {
            Ok(Value::Integer(args[0].as_float()?.ceil() as i64))
        }),
        Builtin::new("compareVersions", &[true, true], |args: Vec<Value>| {
            let s1 = args[0].to_str()?;
            let s1 = VersionPartsIter::new_for_cmp(s1.as_str());
            let s2 = args[1].to_str()?;
            let s2 = VersionPartsIter::new_for_cmp(s2.as_str());

            match s1.cmp(s2) {
                std::cmp::Ordering::Less => Ok(Value::Integer(-1)),
                std::cmp::Ordering::Equal => Ok(Value::Integer(0)),
                std::cmp::Ordering::Greater => Ok(Value::Integer(1)),
            }
        }),
        Builtin::new("concatLists", &[true], |args: Vec<Value>| {
            let list = args[0].to_list()?;
            let lists = list
                .into_iter()
                .map(|elem| {
                    let value = elem.force()?;
                    value.to_list()
                })
                .collect::<Result<Vec<NixList>, ErrorKind>>()?;

            Ok(Value::List(NixList::from(
                lists.into_iter().flatten().collect::<Vec<Value>>(),
            )))
        }),
        Builtin::new("concatMap", &[true, true], |args: Vec<Value>| {
            let list = args[1].to_list()?;
            let mut res = Vec::new();
            for val in list {
                res.extend(VM::vm_call_with(&args[0], [val])?.force()?.to_list()?);
            }
            Ok(Value::List(res.into()))
        }),
        Builtin::new("concatStringsSep", &[true, true], |args: Vec<Value>| {
            let separator = args[0].to_str()?;
            let list = args[1].to_list()?;
            let mut res = String::new();
            for (i, val) in list.into_iter().enumerate() {
                if i != 0 {
                    res.push_str(&separator);
                }
                res.push_str(&val.force()?.coerce_to_string(CoercionKind::Weak)?);
            }
            Ok(res.into())
        }),
        Builtin::new("deepSeq", &[true, true], |mut args: Vec<Value>| {
            let arg2 = args.pop().unwrap();
            let arg1 = args.pop().unwrap();
            arg1.deep_force(&mut Default::default())?;
            Ok(arg2)
        }),
        Builtin::new(
            "div",
            &[false, false],
            |args: Vec<Value>| arithmetic_op!(&*args[0].force()?, &*args[1].force()?, /),
        ),
        Builtin::new("dirOf", &[true], |args: Vec<Value>| {
            let s = args[0].coerce_to_string(CoercionKind::Weak)?;
            let result = s
                .rsplit_once('/')
                .map(|(x, _)| match x {
                    "" => "/",
                    _ => x,
                })
                .unwrap_or(".");
            if args[0].is_path() {
                Ok(Value::Path(result.into()))
            } else {
                Ok(result.into())
            }
        }),
        Builtin::new("elem", &[true, true], |args: Vec<Value>| {
            for val in args[1].to_list()? {
                if val.nix_eq(&args[0])? {
                    return Ok(true.into());
                }
            }
            Ok(false.into())
        }),
        Builtin::new("elemAt", &[true, true], |args: Vec<Value>| {
            let xs = args[0].to_list()?;
            let i = args[1].as_int()?;
            if i < 0 {
                Err(ErrorKind::IndexOutOfBounds { index: i })
            } else {
                match xs.get(i as usize) {
                    Some(x) => Ok(x.clone()),
                    None => Err(ErrorKind::IndexOutOfBounds { index: i }),
                }
            }
        }),
        Builtin::new("filter", &[true, true], |args: Vec<Value>| {
            let list: NixList = args[1].to_list()?;

            list.into_iter()
                .filter_map(|elem| {
                    let result = match VM::vm_call_with(&args[0], [elem.clone()]) {
                        Err(err) => return Some(Err(err)),
                        Ok(result) => result,
                    };

                    // Must be assigned to a local to avoid a borrowcheck
                    // failure related to the ForceResult destructor.
                    let result = match result.force() {
                        Err(err) => Some(Err(VM::vm_error(err))),
                        Ok(value) => match value.as_bool() {
                            Ok(true) => Some(Ok(elem)),
                            Ok(false) => None,
                            Err(err) => Some(Err(VM::vm_error(err))),
                        },
                    };

                    result
                })
                .collect::<Result<Vec<Value>, _>>()
                .map(|list| Value::List(NixList::from(list)))
                .map_err(Into::into)
        }),
        Builtin::new("floor", &[true], |args: Vec<Value>| {
            Ok(Value::Integer(args[0].as_float()?.floor() as i64))
        }),
        Builtin::new("foldl'", &[true, false, true], |mut args: Vec<Value>| {
            let list = args.pop().unwrap().to_list()?;
            let mut res = args.pop().unwrap();
            let op = args.pop().unwrap();
            for val in list {
                res = VM::vm_call_with(&op, [res, val])?;
                res.force()?;
            }

            Ok(res)
        }),
        Builtin::new("functionArgs", &[true], |args: Vec<Value>| {
            let lambda = args[0].to_closure()?.lambda();
            let formals = if let Some(formals) = &lambda.formals {
                formals
            } else {
                return Ok(Value::attrs(NixAttrs::empty()));
            };
            Ok(Value::attrs(NixAttrs::from_map(
                formals
                    .arguments
                    .iter()
                    .map(|(k, v)| (k.clone(), (*v).into()))
                    .collect(),
            )))
        }),
        Builtin::new("fromJSON", &[true], |args: Vec<Value>| {
            let json_str = args[0].to_str()?;
            let json: serde_json::Value = serde_json::from_str(&json_str)?;
            json.try_into()
        }),
        Builtin::new("genList", &[true, true], |args: Vec<Value>| {
            let len = args[1].as_int()?;
            (0..len)
                .map(|i| VM::vm_call_with(&args[0], [i.into()]))
                .collect::<Result<Vec<Value>, _>>()
                .map(|list| Value::List(NixList::from(list)))
                .map_err(Into::into)
        }),
        Builtin::new("getAttr", &[true, true], |args: Vec<Value>| {
            let k = args[0].to_str()?;
            let xs = args[1].to_attrs()?;

            match xs.select(k.as_str()) {
                Some(x) => Ok(x.clone()),
                None => Err(ErrorKind::AttributeNotFound {
                    name: k.to_string(),
                }),
            }
        }),
        Builtin::new("groupBy", &[true, true], |args: Vec<Value>| {
            let mut res: BTreeMap<NixString, Value> = BTreeMap::new();
            for val in args[1].to_list()? {
                let key = VM::vm_call_with(&args[0], [val.clone()])?
                    .force()?
                    .to_str()?;
                res.entry(key)
                    .or_insert_with(|| Value::List(NixList::new()))
                    .as_list_mut()?
                    .push(val);
            }
            Ok(Value::attrs(NixAttrs::from_map(res)))
        }),
        Builtin::new("hasAttr", &[true, true], |args: Vec<Value>| {
            let k = args[0].to_str()?;
            let xs = args[1].to_attrs()?;

            Ok(Value::Bool(xs.contains(k.as_str())))
        }),
        Builtin::new("head", &[true], |args: Vec<Value>| {
            match args[0].to_list()?.get(0) {
                Some(x) => Ok(x.clone()),
                None => Err(ErrorKind::IndexOutOfBounds { index: 0 }),
            }
        }),
        Builtin::new("intersectAttrs", &[true, true], |args: Vec<Value>| {
            let mut res = BTreeMap::new();
            let attrs1 = args[0].to_attrs()?;
            let attrs2 = args[1].to_attrs()?;
            for (k, v) in attrs2.iter() {
                if attrs1.contains(k) {
                    res.insert(k.clone(), v.clone());
                }
            }
            Ok(Value::attrs(NixAttrs::from_map(res)))
        }),
        // For `is*` predicates we force manually, as Value::force also unwraps any Thunks
        Builtin::new("isAttrs", &[false], |args: Vec<Value>| {
            let value = args[0].force()?;
            Ok(Value::Bool(matches!(*value, Value::Attrs(_))))
        }),
        Builtin::new("isBool", &[false], |args: Vec<Value>| {
            let value = args[0].force()?;
            Ok(Value::Bool(matches!(*value, Value::Bool(_))))
        }),
        Builtin::new("isFloat", &[false], |args: Vec<Value>| {
            let value = args[0].force()?;
            Ok(Value::Bool(matches!(*value, Value::Float(_))))
        }),
        Builtin::new("isFunction", &[false], |args: Vec<Value>| {
            let value = args[0].force()?;
            Ok(Value::Bool(matches!(
                *value,
                Value::Closure(_) | Value::Builtin(_)
            )))
        }),
        Builtin::new("isInt", &[false], |args: Vec<Value>| {
            let value = args[0].force()?;
            Ok(Value::Bool(matches!(*value, Value::Integer(_))))
        }),
        Builtin::new("isList", &[false], |args: Vec<Value>| {
            let value = args[0].force()?;
            Ok(Value::Bool(matches!(*value, Value::List(_))))
        }),
        Builtin::new("isNull", &[false], |args: Vec<Value>| {
            let value = args[0].force()?;
            Ok(Value::Bool(matches!(*value, Value::Null)))
        }),
        Builtin::new("isPath", &[false], |args: Vec<Value>| {
            let value = args[0].force()?;
            Ok(Value::Bool(matches!(*value, Value::Path(_))))
        }),
        Builtin::new("isString", &[false], |args: Vec<Value>| {
            let value = args[0].force()?;
            Ok(Value::Bool(matches!(*value, Value::String(_))))
        }),
        Builtin::new("length", &[true], |args: Vec<Value>| {
            Ok(Value::Integer(args[0].to_list()?.len() as i64))
        }),
        Builtin::new("lessThan", &[false, false], |args: Vec<Value>| {
            Ok(Value::Bool(matches!(
                args[0].force()?.nix_cmp(&*args[1].force()?)?,
                Some(Ordering::Less)
            )))
        }),
        Builtin::new("listToAttrs", &[true], |args: Vec<Value>| {
            let list = args[0].to_list()?;
            let mut map = BTreeMap::new();
            for val in list {
                let attrs = val.force()?.to_attrs()?;
                let get = |key| {
                    attrs
                        .select(key)
                        .ok_or(ErrorKind::AttributeNotFound { name: key.into() })
                };
                let name = get("name")?.to_str()?;
                let value = get("value")?.clone();
                // Map entries earlier in the list take precedence over entries later in the list
                map.entry(name).or_insert(value);
            }
            Ok(Value::attrs(NixAttrs::from_map(map)))
        }),
        Builtin::new("map", &[true, true], |args: Vec<Value>| {
            let list: NixList = args[1].to_list()?;

            list.into_iter()
                .map(|val| VM::vm_call_with(&args[0], [val]))
                .collect::<Result<Vec<Value>, _>>()
                .map(|list| Value::List(NixList::from(list)))
                .map_err(Into::into)
        }),
        Builtin::new("mapAttrs", &[true, true], |args: Vec<Value>| {
            let attrs = args[1].to_attrs()?;
            let mut res = BTreeMap::new();
            for (key, value) in attrs.as_ref() {
                let value = VM::vm_call_with(&args[0], [key.clone().into(), value.clone()])?;
                res.insert(key.clone(), value);
            }
            Ok(Value::attrs(NixAttrs::from_map(res)))
        }),
        Builtin::new("match", &[true, true], |mut args: Vec<Value>| {
            let s = args.pop().unwrap().to_str()?;
            let re = args.pop().unwrap().to_str()?;
            let re: Regex = Regex::new(&format!("^{}$", re.as_str())).unwrap();
            match re.captures(&s) {
                Some(caps) => Ok(caps
                    .iter()
                    .skip(1)
                    .map(|grp| grp.map(|g| Value::from(g.as_str())).unwrap_or(Value::Null))
                    .collect::<Vec<Value>>()
                    .into()),
                None => Ok(Value::Null),
            }
        }),
        Builtin::new(
            "mul",
            &[false, false],
            |args: Vec<Value>| arithmetic_op!(&*args[0].force()?, &*args[1].force()?, *),
        ),
        Builtin::new("parseDrvName", &[true], |args: Vec<Value>| {
            // This replicates cppnix's (mis?)handling of codepoints
            // above U+007f following 0x2d ('-')
            let s = args[0].to_str()?;
            let slice: &[u8] = s.as_str().as_ref();
            let (name, dash_and_version) = slice.split_at(
                slice
                    .windows(2)
                    .enumerate()
                    .find_map(|x| match x {
                        (idx, [b'-', c1]) if !c1.is_ascii_alphabetic() => Some(idx),
                        _ => None,
                    })
                    .unwrap_or(slice.len()),
            );
            let version = dash_and_version
                .split_first()
                .map(|x| core::str::from_utf8(x.1))
                .unwrap_or(Ok(""))?
                .into();
            Ok(Value::attrs(NixAttrs::from_map(BTreeMap::from([
                (NixString::NAME, core::str::from_utf8(name)?.into()),
                ("version".into(), version),
            ]))))
        }),
        Builtin::new("partition", &[true, true], |args: Vec<Value>| {
            let mut right: Vec<Value> = vec![];
            let mut wrong: Vec<Value> = vec![];

            let list: NixList = args[1].to_list()?;
            for elem in list.into_iter() {
                let result = VM::vm_call_with(&args[0], [elem.clone()])?;

                if result.force()?.as_bool()? {
                    right.push(elem);
                } else {
                    wrong.push(elem);
                };
            }

            let mut res: BTreeMap<NixString, Value> = BTreeMap::new();
            res.insert("right".into(), Value::List(right.into()));
            res.insert("wrong".into(), Value::List(wrong.into()));

            Ok(Value::attrs(NixAttrs::from_map(res)))
        }),
        Builtin::new("removeAttrs", &[true, true], |args: Vec<Value>| {
            let attrs = args[0].to_attrs()?;
            let keys = args[1]
                .to_list()?
                .into_iter()
                .map(|v| v.to_str())
                .collect::<Result<HashSet<_>, _>>()?;
            let mut res = BTreeMap::new();
            for (k, v) in attrs.iter() {
                if !keys.contains(k) {
                    res.insert(k.clone(), v.clone());
                }
            }
            Ok(Value::attrs(NixAttrs::from_map(res)))
        }),
        Builtin::new("replaceStrings", &[true, true, true], |args: Vec<Value>| {
            let from = args[0].to_list()?;
            from.force_elements()?;
            let to = args[1].to_list()?;
            to.force_elements()?;
            let string = args[2].to_str()?;

            let mut res = String::new();

            let mut i: usize = 0;
            let mut empty_string_replace = false;

            // This can't be implemented using Rust's string.replace() as
            // well as a map because we need to handle errors with results
            // as well as "reset" the iterator to zero for the replacement
            // everytime there's a successful match.
            // Also, Rust's string.replace allocates a new string
            // on every call which is not preferable.
            'outer: while i < string.len() {
                // Try a match in all the from strings
                for elem in std::iter::zip(from.iter(), to.iter()) {
                    let from = elem.0.to_str()?;
                    let to = elem.1.to_str()?;

                    if i + from.len() >= string.len() {
                        continue;
                    }

                    // We already applied a from->to with an empty from
                    // transformation.
                    // Let's skip it so that we don't loop infinitely
                    if empty_string_replace && from.as_str().len() == 0 {
                        continue;
                    }

                    // if we match the `from` string, let's replace
                    if &string[i..i + from.len()] == from.as_str() {
                        res += &to;
                        i += from.len();

                        // remember if we applied the empty from->to
                        empty_string_replace = from.as_str().len() == 0;

                        continue 'outer;
                    }
                }

                // If we don't match any `from`, we simply add a character
                res += &string[i..i + 1];
                i += 1;

                // Since we didn't apply anything transformation,
                // we reset the empty string replacement
                empty_string_replace = false;
            }

            // Special case when the string is empty or at the string's end
            // and one of the from is also empty
            for elem in std::iter::zip(from.iter(), to.iter()) {
                let from = elem.0.to_str()?;
                let to = elem.1.to_str()?;

                if from.as_str().len() == 0 {
                    res += &to;
                    break;
                }
            }
            Ok(Value::String(res.into()))
        }),
        Builtin::new("seq", &[true, true], |mut args: Vec<Value>| {
            // The builtin calling infra has already forced both args for us, so
            // we just return the second and ignore the first
            Ok(args.pop().unwrap())
        }),
        Builtin::new("sort", &[true, true], |args: Vec<Value>| {
            let mut list = args[1].to_list()?;
            let comparator = &args[0];

            // Used to let errors "escape" from the sorting closure. If anything
            // ends up setting an error, it is returned from this function.
            let mut error: Option<ErrorKind> = None;

            list.sort_by(|lhs, rhs| {
                let result = VM::vm_call_with(comparator, [lhs.clone(), rhs.clone()])
                    .map_err(|err| ErrorKind::ThunkForce(Box::new(err)))
                    .and_then(|v| v.force()?.as_bool());

                match (&error, result) {
                    // The contained closure only returns a "less
                    // than?"-boolean, no way to yield "equal".
                    (None, Ok(true)) => Ordering::Less,
                    (None, Ok(false)) => Ordering::Greater,

                    // Closest thing to short-circuiting out if an error was
                    // thrown.
                    (Some(_), _) => Ordering::Equal,

                    // Propagate the error if one was encountered.
                    (_, Err(e)) => {
                        error = Some(e);
                        Ordering::Equal
                    }
                }
            });

            match error {
                None => Ok(Value::List(list)),
                Some(e) => Err(e),
            }
        }),
        Builtin::new("splitVersion", &[true], |args: Vec<Value>| {
            let s = args[0].to_str()?;
            let s = VersionPartsIter::new(s.as_str());

            let parts = s
                .map(|s| {
                    Value::String(match s {
                        VersionPart::Number(n) => n.into(),
                        VersionPart::Word(w) => w.into(),
                    })
                })
                .collect::<Vec<Value>>();
            Ok(Value::List(NixList::construct(parts.len(), parts)))
        }),
        Builtin::new("stringLength", &[false], |args: Vec<Value>| {
            // also forces the value
            let s = args[0].coerce_to_string(CoercionKind::Weak)?;
            Ok(Value::Integer(s.as_str().len() as i64))
        }),
        Builtin::new(
            "sub",
            &[false, false],
            |args: Vec<Value>| arithmetic_op!(&*args[0].force()?, &*args[1].force()?, -),
        ),
        Builtin::new("substring", &[true, true, true], |args: Vec<Value>| {
            let beg = args[0].as_int()?;
            let len = args[1].as_int()?;
            let x = args[2].to_str()?;

            if beg < 0 {
                return Err(ErrorKind::IndexOutOfBounds { index: beg });
            }
            let beg = beg as usize;

            // Nix doesn't assert that the length argument is
            // non-negative when the starting index is GTE the
            // string's length.
            if beg >= x.as_str().len() {
                return Ok(Value::String("".into()));
            }

            if len < 0 {
                return Err(ErrorKind::NegativeLength { length: len });
            }

            let len = len as usize;
            let end = cmp::min(beg + len, x.as_str().len());

            Ok(Value::String(
                x.as_str()[(beg as usize)..(end as usize)].into(),
            ))
        }),
        Builtin::new("tail", &[true], |args: Vec<Value>| {
            let xs = args[0].to_list()?;

            if xs.len() == 0 {
                Err(ErrorKind::TailEmptyList)
            } else {
                let output = xs.into_iter().skip(1).collect::<Vec<_>>();
                Ok(Value::List(NixList::construct(output.len(), output)))
            }
        }),
        Builtin::new("throw", &[true], |args: Vec<Value>| {
            Err(ErrorKind::Throw(args[0].to_str()?.to_string()))
        }),
        // coerce_to_string forces for us
        Builtin::new("toString", &[false], |args: Vec<Value>| {
            args[0]
                .coerce_to_string(CoercionKind::Strong)
                .map(Value::String)
        }),
        Builtin::new("trace", &[true, true], |mut args: Vec<Value>| {
            let value = args.pop().unwrap();
            let trace_value = args.pop().unwrap();
            // TODO(grfn): `trace` should be pluggable and capturable, probably via a method on
            // the VM
            println!("trace: {} :: {}", trace_value, trace_value.type_of());
            Ok(value)
        }),
        Builtin::new("tryEval", &[false], |args: Vec<Value>| {
            let mut res = BTreeMap::new();
            match args[0].force() {
                Ok(value) => {
                    res.insert("value".into(), (*value).clone());
                    res.insert("success".into(), true.into());
                }
                Err(e) if e.is_catchable() => {
                    res.insert("value".into(), false.into());
                    res.insert("success".into(), false.into());
                }
                Err(e) => return Err(e),
            }
            Ok(Value::attrs(NixAttrs::from_map(res)))
        }),
        Builtin::new("toPath", &[false], |args: Vec<Value>| {
            let path: Value = crate::value::canon_path(coerce_value_to_path(&args[0])?).into();
            Ok(path.coerce_to_string(CoercionKind::Weak)?.into())
        }),
        Builtin::new("typeOf", &[false], |args: Vec<Value>| {
            // We force manually here because it also unwraps the Thunk
            // representation, if any.
            // TODO(sterni): it'd be nice if we didn't have to worry about this
            let value = args[0].force()?;
            Ok(Value::String(value.type_of().into()))
        }),
    ]
}

/// Placeholder builtins that technically have a function which we do
/// not yet implement, but which is also not easily observable from
/// within a pure evaluation context.
///
/// These are used as a crutch to make progress on nixpkgs evaluation.
fn placeholders() -> Vec<Builtin> {
    vec![Builtin::new(
        "addErrorContext",
        &[false, false],
        |mut args: Vec<Value>| {
            VM::vm_emit_warning(WarningKind::NotImplemented("builtins.addErrorContext"));
            Ok(args.pop().unwrap())
        },
    )]
}
// we set TVIX_CURRENT_SYSTEM in build.rs
pub const CURRENT_PLATFORM: &str = env!("TVIX_CURRENT_SYSTEM");

/// Set of Nix builtins that are globally available.
pub fn global_builtins(source: SourceCode) -> GlobalsMapFunc {
    Box::new(move |globals: &std::rc::Weak<GlobalsMap>| {
        let mut map: BTreeMap<&'static str, Value> = BTreeMap::new();

        // Pure-value builtins
        map.insert("nixVersion", Value::String("2.3-compat-tvix-0.1".into()));

        map.insert("langVersion", Value::Integer(6));

        map.insert(
            "currentSystem",
            crate::systems::llvm_triple_to_nix_double(CURRENT_PLATFORM).into(),
        );

        let mut add_builtins = |builtins: Vec<Builtin>| {
            for builtin in builtins {
                map.insert(builtin.name(), Value::Builtin(builtin));
            }
        };

        add_builtins(pure_builtins());
        add_builtins(placeholders());

        #[cfg(feature = "impure")]
        {
            map.extend(impure::builtins());

            // We need to insert import into the builtins, but the
            // builtins passed to import must have import *in it*.
            let import = Value::Builtin(crate::builtins::impure::builtins_import(
                globals,
                source.clone(),
            ));

            map.insert("import", import);
        };

        let mut globals: GlobalsMap = HashMap::new();

        let builtins = Rc::new(NixAttrs::from_map(
            map.into_iter().map(|(k, v)| (k.into(), v)).collect(),
        ));

        // known global builtins from the builtins set.
        for global in &[
            "abort",
            "baseNameOf",
            "derivation",
            "derivationStrict",
            "dirOf",
            "fetchGit",
            "fetchMercurial",
            "fetchTarball",
            "fromTOML",
            "import",
            "isNull",
            "map",
            "placeholder",
            "removeAttrs",
            "scopedImport",
            "throw",
            "toString",
        ] {
            if let Some(builtin) = builtins.select(global) {
                let builtin = builtin.clone();
                globals.insert(
                    global,
                    Rc::new(move |c, s| c.emit_constant(builtin.clone(), &s)),
                );
            }
        }

        globals.insert(
            "builtins",
            Rc::new(move |c, s| c.emit_constant(Value::attrs(builtins.as_ref().clone()), &s)),
        );

        globals
    })
}
