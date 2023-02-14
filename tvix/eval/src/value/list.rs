//! This module implements Nix lists.
use std::ops::Index;

use imbl::{vector, Vector};

use serde::{Deserialize, Serialize};

use crate::generators;
use crate::generators::GenCo;
use crate::AddContext;
use crate::ErrorKind;

use super::thunk::ThunkSet;
use super::TotalDisplay;
use super::Value;

#[repr(transparent)]
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct NixList(Vector<Value>);

impl TotalDisplay for NixList {
    fn total_fmt(&self, f: &mut std::fmt::Formatter<'_>, set: &mut ThunkSet) -> std::fmt::Result {
        f.write_str("[ ")?;

        for v in self {
            v.total_fmt(f, set)?;
            f.write_str(" ")?;
        }

        f.write_str("]")
    }
}

impl From<Vector<Value>> for NixList {
    fn from(vs: Vector<Value>) -> Self {
        Self(vs)
    }
}

impl NixList {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, i: usize) -> Option<&Value> {
        self.0.get(i)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn construct(count: usize, stack_slice: Vec<Value>) -> Self {
        debug_assert!(
            count == stack_slice.len(),
            "NixList::construct called with count == {}, but slice.len() == {}",
            count,
            stack_slice.len(),
        );

        NixList(Vector::from_iter(stack_slice.into_iter()))
    }

    pub fn iter(&self) -> vector::Iter<Value> {
        self.0.iter()
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.0.ptr_eq(&other.0)
    }

    pub fn into_inner(self) -> Vector<Value> {
        self.0
    }

    #[deprecated(note = "callers should avoid constructing from Vec")]
    pub fn from_vec(vs: Vec<Value>) -> Self {
        Self(Vector::from_iter(vs.into_iter()))
    }

    /// Asynchronous sorting algorithm in which the comparator can make use of
    /// VM requests (required as `builtins.sort` uses comparators written in
    /// Nix).
    ///
    /// The algorithm here is merge sort, as it provides a stable sort without
    /// recursion. There's quite a bit of optimisation potential by
    /// incorporating, for example, the adaptations present in the Rust standard
    /// library's implementation of it. For now it just needs to work.
    pub async fn sort_by(&mut self, co: &GenCo, cmp: Value) -> Result<(), ErrorKind> {
        let n = self.len();
        let mut len = 1;

        while len < n {
            let mut i = 0;
            while i < n {
                let l_1 = i;
                let r_1 = i + len - 1;
                let l_2 = i + len;
                let mut r_2 = i + 2 * len - 1;

                if l_2 >= n {
                    break;
                }

                if r_2 >= n {
                    r_2 = n - 1;
                }

                let temp = merge(co, &self.0, &cmp, l_1, r_1, l_2, r_2).await?;
                for j in 0..(r_2 - l_1 + 1) {
                    self.0[i + j] = temp[j].clone();
                }

                i = i + 2 * len;
            }
            len *= 2;
        }

        Ok(())
    }
}

// Helper functions for list sorting.
async fn call_comparator(co: &GenCo, cmp: &Value, a: &Value, b: &Value) -> Result<bool, ErrorKind> {
    generators::request_force(
        co,
        generators::request_call_with(co, cmp.clone(), [a.clone(), b.clone()]).await,
    )
    .await
    .as_bool()
    .context("evaluating comparison function in `builtins.sort`")
}

// /// The "merge" in "merge-sort".
// ///
// /// Based on https://github.com/TheAlgorithms/Rust/blob/master/src/sorting/merge_sort.rs.
// async fn merge(co: &GenCo, cmp: &Value, list: &mut [Value], mid: usize) -> Result<(), ErrorKind> {
//     let left_half = list[..mid].to_vec();
//     let right_half = list[mid..].to_vec();

//     let mut l = 0;
//     let mut r = 0;

//     for v in list {
//         if r == right_half.len()
//             || (l < left_half.len()
//                 && call_comparator(co, cmp, left_half[l].clone(), right_half[r].clone()).await?)
//         {
//             *v = left_half[l].clone();
//             l += 1;
//         } else {
//             *v = right_half[r].clone();
//             r += 1;
//         }
//     }

//     Ok(())
// }

/// The "merge" in "merge-sort".
async fn merge<'l>(
    co: &GenCo,
    data: &Vector<Value>,
    cmp: &Value,
    mut l_1: usize,
    r_1: usize,
    mut l_2: usize,
    r_2: usize,
) -> Result<Vector<Value>, ErrorKind> {
    let mut tmp = Vector::new();

    while l_1 <= r_1 && l_2 <= r_2 {
        if call_comparator(co, cmp, &data[l_1], &data[l_2]).await? {
            tmp.push_back(data[l_1].clone());
            l_1 += 1;
        } else {
            tmp.push_back(data[l_2].clone());
            l_2 += 1;
        }
    }

    while l_1 <= r_1 {
        tmp.push_back(data[l_1].clone());
        l_1 += 1;
    }

    while l_2 <= r_2 {
        tmp.push_back(data[l_2].clone());
        l_2 += 1;
    }

    Ok(tmp)
}

impl IntoIterator for NixList {
    type Item = Value;
    type IntoIter = imbl::vector::ConsumingIter<Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a NixList {
    type Item = &'a Value;
    type IntoIter = imbl::vector::Iter<'a, Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl Index<usize> for NixList {
    type Output = Value;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}
