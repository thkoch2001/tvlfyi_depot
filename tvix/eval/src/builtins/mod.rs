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

use builtin_macros::builtins;
use regex::Regex;

use crate::warnings::WarningKind;
use crate::{
    errors::ErrorKind,
    value::{Builtin, CoercionKind, NixAttrs, NixList, NixString, Value},
    vm::VM,
};

use crate::{arithmetic_op, unwrap_or_clone_rc};

use self::versions::{VersionPart, VersionPartsIter};

#[cfg(feature = "impure")]
pub mod impure;
pub mod versions;

/// Coerce a Nix Value to a plain path, e.g. in order to access the
/// file it points to via either `builtins.toPath` or an impure
/// builtin. This coercion can _never_ be performed in a Nix program
/// without using builtins (i.e. the trick `path: /. + path` to
/// convert from a string to a path wouldn't hit this code).
pub fn coerce_value_to_path(v: &Value, vm: &mut VM) -> Result<PathBuf, ErrorKind> {
    let value = v.force(vm)?;
    match &*value {
        Value::Thunk(t) => coerce_value_to_path(&t.value(), vm),
        Value::Path(p) => Ok(p.clone()),
        _ => value
            .coerce_to_string(CoercionKind::Weak, vm)
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

#[builtins]
mod pure_builtins {
    use super::*;

    #[builtin("abort")]
    fn builtin_abort(_vm: &mut VM, message: Value) -> Result<Value, ErrorKind> {
        Err(ErrorKind::Abort(message.to_str()?.to_string()))
    }

    #[builtin("add", [false, false])]
    fn builtin_add(vm: &mut VM, x: Value, y: Value) -> Result<Value, ErrorKind> {
        arithmetic_op!(&*x.force(vm)?, &*y.force(vm)?, +)
    }

    #[builtin("all", [true, true])]
    fn builtin_all(vm: &mut VM, pred: Value, list: Value) -> Result<Value, ErrorKind> {
        for value in list.to_list()?.into_iter() {
            let pred_result = vm.call_with(&pred, [value])?;

            if !pred_result.force(vm)?.as_bool()? {
                return Ok(Value::Bool(false));
            }
        }

        Ok(Value::Bool(true))
    }

    #[builtin("any", [true, true])]
    fn builtin_any(vm: &mut VM, pred: Value, list: Value) -> Result<Value, ErrorKind> {
        for value in list.to_list()?.into_iter() {
            let pred_result = vm.call_with(&pred, [value])?;

            if pred_result.force(vm)?.as_bool()? {
                return Ok(Value::Bool(true));
            }
        }

        Ok(Value::Bool(false))
    }

    #[builtin("attrNames", [true])]
    fn builtin_attr_names(_: &mut VM, set: Value) -> Result<Value, ErrorKind> {
        let xs = set.to_attrs()?;
        let mut output = Vec::with_capacity(xs.len());

        for (key, _val) in xs.iter() {
            output.push(Value::String(key.clone()));
        }

        Ok(Value::List(NixList::construct(output.len(), output)))
    }

    #[builtin("attrValues", [true])]
    fn builtin_attr_values(_: &mut VM, set: Value) -> Result<Value, ErrorKind> {
        let xs = set.to_attrs()?;
        let mut output = Vec::with_capacity(xs.len());

        for (_key, val) in xs.iter() {
            output.push(val.clone());
        }

        Ok(Value::List(NixList::construct(output.len(), output)))
    }

    #[builtin("baseNameOf", [true])]
    fn builtin_base_name_of(vm: &mut VM, s: Value) -> Result<Value, ErrorKind> {
        let s = s.coerce_to_string(CoercionKind::Weak, vm)?;
        let result: String = s.rsplit_once('/').map(|(_, x)| x).unwrap_or(&s).into();
        Ok(result.into())
    }

    #[builtin("bitAnd", [true, true])]
    fn builtin_bit_and(_: &mut VM, x: Value, y: Value) -> Result<Value, ErrorKind> {
        Ok(Value::Integer(x.as_int()? & y.as_int()?))
    }

    #[builtin("bitOr", [true, true])]
    fn builtin_bit_or(_: &mut VM, x: Value, y: Value) -> Result<Value, ErrorKind> {
        Ok(Value::Integer(x.as_int()? | y.as_int()?))
    }

    #[builtin("bitXor", [true, true])]
    fn builtin_bit_xor(_: &mut VM, x: Value, y: Value) -> Result<Value, ErrorKind> {
        Ok(Value::Integer(x.as_int()? ^ y.as_int()?))
    }

    #[builtin("catAttrs", [true, true])]
    fn builtin_cat_attrs(vm: &mut VM, key: Value, list: Value) -> Result<Value, ErrorKind> {
        let key = key.to_str()?;
        let list = list.to_list()?;
        let mut output = vec![];

        for item in list.into_iter() {
            let set = item.force(vm)?.to_attrs()?;
            if let Some(value) = set.select(key.as_str()) {
                output.push(value.clone());
            }
        }

        Ok(Value::List(NixList::construct(output.len(), output)))
    }

    #[builtin("ceil", [true])]
    fn builtin_ceil(_: &mut VM, double: Value) -> Result<Value, ErrorKind> {
        Ok(Value::Integer(double.as_float()?.ceil() as i64))
    }

    #[builtin("compareVersions", [true, true])]
    fn builtin_compare_versions(_: &mut VM, x: Value, y: Value) -> Result<Value, ErrorKind> {
        let s1 = x.to_str()?;
        let s1 = VersionPartsIter::new_for_cmp(s1.as_str());
        let s2 = y.to_str()?;
        let s2 = VersionPartsIter::new_for_cmp(s2.as_str());

        match s1.cmp(s2) {
            std::cmp::Ordering::Less => Ok(Value::Integer(-1)),
            std::cmp::Ordering::Equal => Ok(Value::Integer(0)),
            std::cmp::Ordering::Greater => Ok(Value::Integer(1)),
        }
    }

    #[builtin("concatLists", [true])]
    fn builtin_concat_lists(vm: &mut VM, lists: Value) -> Result<Value, ErrorKind> {
        let list = lists.to_list()?;
        let lists = list
            .into_iter()
            .map(|elem| {
                let value = elem.force(vm)?;
                value.to_list()
            })
            .collect::<Result<Vec<NixList>, ErrorKind>>()?;

        Ok(Value::List(NixList::from(
            lists.into_iter().flatten().collect::<Vec<Value>>(),
        )))
    }

    #[builtin("concatMap", [true, true])]
    fn builtin_concat_map(vm: &mut VM, f: Value, list: Value) -> Result<Value, ErrorKind> {
        let list = list.to_list()?;
        let mut res = Vec::new();
        for val in list {
            res.extend(vm.call_with(&f, [val])?.force(vm)?.to_list()?);
        }
        Ok(Value::List(res.into()))
    }

    #[builtin("concatStringsSep", [true, true])]
    fn builtin_concat_strings_sep(
        vm: &mut VM,
        separator: Value,
        list: Value,
    ) -> Result<Value, ErrorKind> {
        let separator = separator.to_str()?;
        let list = list.to_list()?;
        let mut res = String::new();
        for (i, val) in list.into_iter().enumerate() {
            if i != 0 {
                res.push_str(&separator);
            }
            res.push_str(&val.force(vm)?.coerce_to_string(CoercionKind::Weak, vm)?);
        }
        Ok(res.into())
    }

    #[builtin("deepSeq", [true, true])]
    fn builtin_deep_seq(vm: &mut VM, x: Value, y: Value) -> Result<Value, ErrorKind> {
        x.deep_force(vm, &mut Default::default())?;
        Ok(y)
    }

    #[builtin("div", [false, false])]
    fn builtin_div(vm: &mut VM, x: Value, y: Value) -> Result<Value, ErrorKind> {
        arithmetic_op!(&*x.force(vm)?, &*y.force(vm)?, /)
    }

    #[builtin("dirOf", [true])]
    fn builtin_dir_of(vm: &mut VM, s: Value) -> Result<Value, ErrorKind> {
        let str = s.coerce_to_string(CoercionKind::Weak, vm)?;
        let result = str
            .rsplit_once('/')
            .map(|(x, _)| match x {
                "" => "/",
                _ => x,
            })
            .unwrap_or(".");
        if s.is_path() {
            Ok(Value::Path(result.into()))
        } else {
            Ok(result.into())
        }
    }

    #[builtin("elem", [true, true])]
    fn builtin_elem(vm: &mut VM, x: Value, xs: Value) -> Result<Value, ErrorKind> {
        for val in xs.to_list()? {
            if val.nix_eq(&x, vm)? {
                return Ok(true.into());
            }
        }
        Ok(false.into())
    }

    #[builtin("elemAt", [true, true])]
    fn builtin_elem_at(_: &mut VM, xs: Value, i: Value) -> Result<Value, ErrorKind> {
        let xs = xs.to_list()?;
        let i = i.as_int()?;
        if i < 0 {
            Err(ErrorKind::IndexOutOfBounds { index: i })
        } else {
            match xs.get(i as usize) {
                Some(x) => Ok(x.clone()),
                None => Err(ErrorKind::IndexOutOfBounds { index: i }),
            }
        }
    }

    #[builtin("filter", [true, true])]
    fn builtin_filter(vm: &mut VM, pred: Value, list: Value) -> Result<Value, ErrorKind> {
        let list: NixList = list.to_list()?;

        list.into_iter()
            .filter_map(|elem| {
                let result = match vm.call_with(&pred, [elem.clone()]) {
                    Err(err) => return Some(Err(err)),
                    Ok(result) => result,
                };

                // Must be assigned to a local to avoid a borrowcheck
                // failure related to the ForceResult destructor.
                let result = match result.force(vm) {
                    Err(err) => Some(Err(vm.error(err))),
                    Ok(value) => match value.as_bool() {
                        Ok(true) => Some(Ok(elem)),
                        Ok(false) => None,
                        Err(err) => Some(Err(vm.error(err))),
                    },
                };

                result
            })
            .collect::<Result<Vec<Value>, _>>()
            .map(|list| Value::List(NixList::from(list)))
            .map_err(Into::into)
    }

    #[builtin("floor", [true])]
    fn builtin_floor(_: &mut VM, double: Value) -> Result<Value, ErrorKind> {
        Ok(Value::Integer(double.as_float()?.floor() as i64))
    }

    #[builtin("foldl'", [true, false, true])]
    fn builtin_foldl(
        vm: &mut VM,
        op: Value,
        mut nul: Value,
        list: Value,
    ) -> Result<Value, ErrorKind> {
        let list = list.to_list()?;
        for val in list {
            nul = vm.call_with(&op, [nul, val])?;
            nul.force(vm)?;
        }

        Ok(nul)
    }

    #[builtin("functionArgs", [true])]
    fn builtin_function_args(_: &mut VM, f: Value) -> Result<Value, ErrorKind> {
        let lambda = f.to_closure()?.lambda();
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
    }

    #[builtin("fromJSON", [true])]
    fn builtin_from_json(_: &mut VM, json: Value) -> Result<Value, ErrorKind> {
        let json_str = json.to_str()?;
        let json: serde_json::Value = serde_json::from_str(&json_str)?;
        json.try_into()
    }

    #[builtin("genList", [true, true])]
    fn builtin_gen_list(vm: &mut VM, generator: Value, length: Value) -> Result<Value, ErrorKind> {
        let len = length.as_int()?;
        (0..len)
            .map(|i| vm.call_with(&generator, [i.into()]))
            .collect::<Result<Vec<Value>, _>>()
            .map(|list| Value::List(NixList::from(list)))
            .map_err(Into::into)
    }

    #[builtin("getAttr", [true, true])]
    fn builtin_get_attr(_: &mut VM, key: Value, set: Value) -> Result<Value, ErrorKind> {
        let k = key.to_str()?;
        let xs = set.to_attrs()?;

        match xs.select(k.as_str()) {
            Some(x) => Ok(x.clone()),
            None => Err(ErrorKind::AttributeNotFound {
                name: k.to_string(),
            }),
        }
    }

    #[builtin("groupBy", [true, true])]
    fn builtin_group_by(vm: &mut VM, f: Value, list: Value) -> Result<Value, ErrorKind> {
        let mut res: BTreeMap<NixString, Value> = BTreeMap::new();
        for val in list.to_list()? {
            let key = vm.call_with(&f, [val.clone()])?.force(vm)?.to_str()?;
            res.entry(key)
                .or_insert_with(|| Value::List(NixList::new()))
                .as_list_mut()?
                .push(val);
        }
        Ok(Value::attrs(NixAttrs::from_map(res)))
    }

    #[builtin("hasAttr", [true, true])]
    fn builtin_has_attr(_: &mut VM, key: Value, set: Value) -> Result<Value, ErrorKind> {
        let k = key.to_str()?;
        let xs = set.to_attrs()?;

        Ok(Value::Bool(xs.contains(k.as_str())))
    }

    #[builtin("head", [true])]
    fn builtin_head(_: &mut VM, list: Value) -> Result<Value, ErrorKind> {
        match list.to_list()?.get(0) {
            Some(x) => Ok(x.clone()),
            None => Err(ErrorKind::IndexOutOfBounds { index: 0 }),
        }
    }

    #[builtin("intersectAttrs", [true, true])]
    fn builtin_intersect_attrs(_: &mut VM, x: Value, y: Value) -> Result<Value, ErrorKind> {
        let mut res = BTreeMap::new();
        let attrs1 = x.to_attrs()?;
        let attrs2 = y.to_attrs()?;
        for (k, v) in attrs2.iter() {
            if attrs1.contains(k) {
                res.insert(k.clone(), v.clone());
            }
        }
        Ok(Value::attrs(NixAttrs::from_map(res)))
    }

    // For `is*` predicates we force manually, as Value::force also unwraps any Thunks

    #[builtin("isAttrs", [false])]
    fn builtin_is_attrs(vm: &mut VM, x: Value) -> Result<Value, ErrorKind> {
        let value = x.force(vm)?;
        Ok(Value::Bool(matches!(*value, Value::Attrs(_))))
    }

    #[builtin("isBool", [false])]
    fn builtin_is_bool(vm: &mut VM, x: Value) -> Result<Value, ErrorKind> {
        let value = x.force(vm)?;
        Ok(Value::Bool(matches!(*value, Value::Bool(_))))
    }

    #[builtin("isFloat", [false])]
    fn builtin_is_float(vm: &mut VM, x: Value) -> Result<Value, ErrorKind> {
        let value = x.force(vm)?;
        Ok(Value::Bool(matches!(*value, Value::Float(_))))
    }

    #[builtin("isFunction", [false])]
    fn builtin_is_function(vm: &mut VM, x: Value) -> Result<Value, ErrorKind> {
        let value = x.force(vm)?;
        Ok(Value::Bool(matches!(
            *value,
            Value::Closure(_) | Value::Builtin(_)
        )))
    }

    #[builtin("isInt", [false])]
    fn builtin_is_int(vm: &mut VM, x: Value) -> Result<Value, ErrorKind> {
        let value = x.force(vm)?;
        Ok(Value::Bool(matches!(*value, Value::Integer(_))))
    }

    #[builtin("isList", [false])]
    fn builtin_is_list(vm: &mut VM, x: Value) -> Result<Value, ErrorKind> {
        let value = x.force(vm)?;
        Ok(Value::Bool(matches!(*value, Value::List(_))))
    }

    #[builtin("isNull", [false])]
    fn builtin_is_null(vm: &mut VM, x: Value) -> Result<Value, ErrorKind> {
        let value = x.force(vm)?;
        Ok(Value::Bool(matches!(*value, Value::Null)))
    }

    #[builtin("isPath", [false])]
    fn builtin_is_path(vm: &mut VM, x: Value) -> Result<Value, ErrorKind> {
        let value = x.force(vm)?;
        Ok(Value::Bool(matches!(*value, Value::Path(_))))
    }

    #[builtin("isString", [false])]
    fn builtin_is_string(vm: &mut VM, x: Value) -> Result<Value, ErrorKind> {
        let value = x.force(vm)?;
        Ok(Value::Bool(matches!(*value, Value::String(_))))
    }

    #[builtin("length", [true])]
    fn builtin_length(_: &mut VM, list: Value) -> Result<Value, ErrorKind> {
        Ok(Value::Integer(list.to_list()?.len() as i64))
    }

    #[builtin("lessThan", [false, false])]
    fn builtin_less_than(vm: &mut VM, x: Value, y: Value) -> Result<Value, ErrorKind> {
        Ok(Value::Bool(matches!(
            x.force(vm)?.nix_cmp(&*y.force(vm)?, vm)?,
            Some(Ordering::Less)
        )))
    }

    #[builtin("listToAttrs", [true])]
    fn builtin_list_to_attrs(vm: &mut VM, list: Value) -> Result<Value, ErrorKind> {
        let list = list.to_list()?;
        let mut map = BTreeMap::new();
        for val in list {
            let attrs = val.force(vm)?.to_attrs()?;
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
    }

    #[builtin("map", [true, true])]
    fn builtin_map(vm: &mut VM, f: Value, list: Value) -> Result<Value, ErrorKind> {
        let list: NixList = list.to_list()?;

        list.into_iter()
            .map(|val| vm.call_with(&f, [val]))
            .collect::<Result<Vec<Value>, _>>()
            .map(|list| Value::List(NixList::from(list)))
            .map_err(Into::into)
    }

    #[builtin("mapAttrs", [true, true])]
    fn builtin_map_attrs(vm: &mut VM, f: Value, attrs: Value) -> Result<Value, ErrorKind> {
        let attrs = attrs.to_attrs()?;
        let mut res = BTreeMap::new();
        for (key, value) in attrs.as_ref() {
            let value = vm.call_with(&f, [key.clone().into(), value.clone()])?;
            res.insert(key.clone(), value);
        }
        Ok(Value::attrs(NixAttrs::from_map(res)))
    }

    #[builtin("match", [true, true])]
    fn builtin_match(_: &mut VM, regex: Value, str: Value) -> Result<Value, ErrorKind> {
        let s = str.to_str()?;
        let re = regex.to_str()?;
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
    }

    #[builtin("mul", [false, false])]
    fn builtin_mul(vm: &mut VM, x: Value, y: Value) -> Result<Value, ErrorKind> {
        arithmetic_op!(&*x.force(vm)?, &*y.force(vm)?, *)
    }

    #[builtin("parseDrvName", [true])]
    fn builtin_parse_drv_name(_vm: &mut VM, s: Value) -> Result<Value, ErrorKind> {
        // This replicates cppnix's (mis?)handling of codepoints
        // above U+007f following 0x2d ('-')
        let s = s.to_str()?;
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
    }
    #[builtin("partition", [true, true])]
    fn builtin_partition(vm: &mut VM, pred: Value, list: Value) -> Result<Value, ErrorKind> {
        let mut right: Vec<Value> = vec![];
        let mut wrong: Vec<Value> = vec![];

        let list: NixList = list.to_list()?;
        for elem in list.into_iter() {
            let result = vm.call_with(&pred, [elem.clone()])?;

            if result.force(vm)?.as_bool()? {
                right.push(elem);
            } else {
                wrong.push(elem);
            };
        }

        let mut res: BTreeMap<NixString, Value> = BTreeMap::new();
        res.insert("right".into(), Value::List(right.into()));
        res.insert("wrong".into(), Value::List(wrong.into()));

        Ok(Value::attrs(NixAttrs::from_map(res)))
    }

    #[builtin("removeAttrs", [true, true])]
    fn builtin_remove_attrs(_: &mut VM, attrs: Value, keys: Value) -> Result<Value, ErrorKind> {
        let attrs = attrs.to_attrs()?;
        let keys = keys
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
    }

    #[builtin("replaceStrings", [true, true, true])]
    fn builtin_replace_strings(
        vm: &mut VM,
        from: Value,
        to: Value,
        s: Value,
    ) -> Result<Value, ErrorKind> {
        let from = from.to_list()?;
        from.force_elements(vm)?;
        let to = to.to_list()?;
        to.force_elements(vm)?;
        let string = s.to_str()?;

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
    }

    #[builtin("seq", [true, true])]
    fn builtin_seq(_: &mut VM, _x: Value, y: Value) -> Result<Value, ErrorKind> {
        // The builtin calling infra has already forced both args for us, so
        // we just return the second and ignore the first
        Ok(y)
    }

    #[builtin("split", [true, true])]
    fn builtin_split(_: &mut VM, regex: Value, str: Value) -> Result<Value, ErrorKind> {
        let s = str.to_str()?;
        let text = s.as_str();
        let re = regex.to_str()?;
        let re: Regex = Regex::new(re.as_str()).unwrap();
        let mut capture_locations = re.capture_locations();
        let num_captures = capture_locations.len();
        let mut ret = NixList::new();
        let mut pos = 0;

        while let Some(thematch) = re.captures_read_at(&mut capture_locations, text, pos) {
            // push the unmatched characters preceding the match
            ret.push(Value::from(&text[pos..thematch.start()]));

            // Push a list with one element for each capture
            // group in the regex, containing the characters
            // matched by that capture group, or null if no match.
            // We skip capture 0; it represents the whole match.
            let v: Vec<Value> = (1..num_captures)
                .map(|i| capture_locations.get(i))
                .map(|o| {
                    o.map(|(start, end)| Value::from(&text[start..end]))
                        .unwrap_or(Value::Null)
                })
                .collect();
            ret.push(Value::List(NixList::from(v)));
            pos = thematch.end();
        }

        // push the unmatched characters following the last match
        ret.push(Value::from(&text[pos..]));

        Ok(Value::List(ret))
    }

    #[builtin("sort", [true, true])]
    fn builtin_sort(vm: &mut VM, comparator: Value, list: Value) -> Result<Value, ErrorKind> {
        let mut list = list.to_list()?;

        // Used to let errors "escape" from the sorting closure. If anything
        // ends up setting an error, it is returned from this function.
        let mut error: Option<ErrorKind> = None;

        list.sort_by(|lhs, rhs| {
            let result = vm
                .call_with(&comparator, [lhs.clone(), rhs.clone()])
                .map_err(|err| ErrorKind::ThunkForce(Box::new(err)))
                .and_then(|v| v.force(vm)?.as_bool());

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
    }

    #[builtin("splitVersion", [true])]
    fn builtin_split_version(_: &mut VM, s: Value) -> Result<Value, ErrorKind> {
        let s = s.to_str()?;
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
    }

    #[builtin("stringLength", [false])]
    fn builtin_string_length(vm: &mut VM, s: Value) -> Result<Value, ErrorKind> {
        // also forces the value
        let s = s.coerce_to_string(CoercionKind::Weak, vm)?;
        Ok(Value::Integer(s.as_str().len() as i64))
    }

    #[builtin("sub", [false, false])]
    fn builtin_sub(vm: &mut VM, x: Value, y: Value) -> Result<Value, ErrorKind> {
        arithmetic_op!(&*x.force(vm)?, &*y.force(vm)?, -)
    }

    #[builtin("substring", [true, true, true])]
    fn builtin_substring(
        _: &mut VM,
        start: Value,
        len: Value,
        s: Value,
    ) -> Result<Value, ErrorKind> {
        let beg = start.as_int()?;
        let len = len.as_int()?;
        let x = s.to_str()?;

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
    }

    #[builtin("tail", [true])]
    fn builtin_tail(_: &mut VM, list: Value) -> Result<Value, ErrorKind> {
        let xs = list.to_list()?;

        if xs.len() == 0 {
            Err(ErrorKind::TailEmptyList)
        } else {
            let output = xs.into_iter().skip(1).collect::<Vec<_>>();
            Ok(Value::List(NixList::construct(output.len(), output)))
        }
    }

    #[builtin("throw", [true])]
    fn builtin_throw(_: &mut VM, message: Value) -> Result<Value, ErrorKind> {
        Err(ErrorKind::Throw(message.to_str()?.to_string()))
    }

    #[builtin("toString", [false])]
    fn builtin_to_string(vm: &mut VM, x: Value) -> Result<Value, ErrorKind> {
        // coerce_to_string forces for us
        x.coerce_to_string(CoercionKind::Strong, vm)
            .map(Value::String)
    }

    #[builtin("trace", [true, true])]
    fn builtin_trace(_: &mut VM, message: Value, value: Value) -> Result<Value, ErrorKind> {
        // TODO(grfn): `trace` should be pluggable and capturable, probably via a method on
        // the VM
        println!("trace: {} :: {}", message, message.type_of());
        Ok(value)
    }

    #[builtin("toPath", [false])]
    fn builtin_to_path(vm: &mut VM, s: Value) -> Result<Value, ErrorKind> {
        let path: Value = crate::value::canon_path(coerce_value_to_path(&s, vm)?).into();
        Ok(path.coerce_to_string(CoercionKind::Weak, vm)?.into())
    }

    #[builtin("tryEval", [false])]
    fn builtin_try_eval(vm: &mut VM, e: Value) -> Result<Value, ErrorKind> {
        let mut res = BTreeMap::new();
        match e.force(vm) {
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
    }

    #[builtin("typeOf", [false])]
    fn builtin_type_of(vm: &mut VM, x: Value) -> Result<Value, ErrorKind> {
        // We force manually here because it also unwraps the Thunk
        // representation, if any.
        // TODO(sterni): it'd be nice if we didn't have to worry about this
        let value = x.force(vm)?;
        Ok(Value::String(value.type_of().into()))
    }
}

pub use pure_builtins::builtins as pure_builtins;

/// Placeholder builtins that technically have a function which we do
/// not yet implement, but which is also not easily observable from
/// within a pure evaluation context.
///
/// These are used as a crutch to make progress on nixpkgs evaluation.
fn placeholders() -> Vec<Builtin> {
    vec![
        Builtin::new(
            "addErrorContext",
            &[false, false],
            |mut args: Vec<Value>, vm: &mut VM| {
                vm.emit_warning(WarningKind::NotImplemented("builtins.addErrorContext"));
                Ok(args.pop().unwrap())
            },
        ),
        Builtin::new(
            "unsafeDiscardStringContext",
            &[true],
            |mut args: Vec<Value>, vm: &mut VM| {
                vm.emit_warning(WarningKind::NotImplemented(
                    "builtins.unsafeDiscardStringContext",
                ));
                Ok(args.pop().unwrap())
            },
        ),
        Builtin::new("derivation", &[true], |args: Vec<Value>, vm: &mut VM| {
            vm.emit_warning(WarningKind::NotImplemented("builtins.derivation"));

            // We do not implement derivations yet, so this function sets mock
            // values on the fields that a real derivation would contain.
            //
            // Crucially this means we do not yet *validate* the values either.
            let attrs = unwrap_or_clone_rc(args[0].to_attrs()?);
            let attrs = attrs.update(NixAttrs::from_map(BTreeMap::from([
                (
                    "outPath".into(),
                    "/nix/store/00000000000000000000000000000000-mock".into(),
                ),
                (
                    "drvPath".into(),
                    "/nix/store/00000000000000000000000000000000-mock.drv".into(),
                ),
                ("type".into(), "derivation".into()),
            ])));

            Ok(Value::Attrs(Rc::new(attrs)))
        }),
    ]
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
