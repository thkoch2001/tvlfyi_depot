//! This module defines the [Tag] trait and the [make] macro,
//! which provide efficient ways to match on tokens that can be distinguished by
//! a single discriminant byte.

/// A Tag is an enumeration of byte strings that in a particular position all
/// have a unique byte.
pub trait Tag: Sized {
    /// Discriminant offset
    const OFF: usize;
    /// Minimum variant length
    const MIN: usize;

    /// Minimal suitably sized buffer for reading the wire representation
    /// HACK: This is a workaround for const generics limitations.
    type Buf: AsMut<[u8]> + Send;

    /// Make an instance of [Self::Buf].
    fn make_buf() -> Self::Buf;

    /// Convert a discriminant into the corresponding variant.
    fn from_u8(x: u8) -> Option<Self>;

    /// Convert a variant back into the wire representation.
    fn as_bytes(&self) -> &'static [u8];
}

/// Consumes an enum structure as described in the below grammar, and rewrites
/// it.
/// It validates that the `$off` specified points to a position
/// in `$TOK` (normally a [u8]) that is a unique discriminant.
/// Specifying a non-unique discriminant is a compile-time error.
macro_rules! make {
    (
        $(
            $vis:vis enum $Enum:ident[$off:expr] {
                $($Var:ident = $TOK:ident,)+
            }
        )*
    ) => {
        $(
            #[derive(Debug, PartialEq, Eq)]
            #[repr(u8)]
            $vis enum $Enum {
                $($Var = $TOK[$Enum::OFF]),+
            }

            impl Tag for $Enum {
                /// Discriminant offset
                const OFF: usize = $off;
                /// Minimum variant length
                const MIN: usize = tag::min_of(&[$($TOK.len()),+]);

                /// Minimal suitably sized buffer for reading the wire representation
                type Buf = [u8; tag::buf_of(&[$($TOK.len()),+])];

                /// Make an instance of [Self::Buf].
                #[inline(always)]
                fn make_buf() -> Self::Buf {
                    [0u8; tag::buf_of(&[$($TOK.len()),+])]
                }

                /// Convert a discriminant into the corresponding variant.
                #[inline(always)]
                fn from_u8(x: u8) -> Option<Self> {
                    #[allow(non_upper_case_globals)]
                    mod __variant {
                        $(
                            pub const $Var: u8 = super::$Enum::$Var as u8;
                        )+
                    }

                    match x {
                        $(__variant::$Var => Some(Self::$Var),)+
                        _ => None
                    }
                }

                /// Convert a variant back into the wire representation.
                #[inline(always)]
                fn as_bytes(&self) -> &'static [u8] {
                    match self {
                        $(Self::$Var => &$TOK,)+
                    }
                }
            }
        )*
    };
}

pub(crate) use make;

/// Get the largest value in a slice.
const fn max_of(mut xs: &[usize]) -> usize {
    let mut y = usize::MIN;
    while let &[x, ref tail @ ..] = xs {
        y = if x > y { x } else { y };
        xs = tail;
    }
    y
}

/// Get the smallest value in a slice.
pub const fn min_of(mut xs: &[usize]) -> usize {
    let mut y = usize::MAX;
    while let &[x, ref tail @ ..] = xs {
        y = if x < y { x } else { y };
        xs = tail;
    }
    y
}

/// Returns a buffer size that's enough to read the first half (until MIN), and
/// the second half.
///
/// Example:
/// ```plain
/// OFF = 16
/// MIN = 24
/// MAX = 64
///
/// BUF = max(MIN, MAX-MIN)
///     = max(24, 64-24)
///     = max(24, 40)
///     = 40
/// ```
pub const fn buf_of(xs: &[usize]) -> usize {
    max_of(&[min_of(xs), max_of(xs) - min_of(xs)])
}
