pub trait Tag: Sized {
    /// Discriminant offset
    const OFF: usize;
    /// Minimum variant length
    const MIN: usize;

    /// Minimal suitably sized buffer for reading the wire representation
    /// HACK: This is a workaround for const generics limitations.
    type Buf: AsMut<[u8]> + Send;

    /// Make an instance of [Self::Buf]
    fn make_buf() -> Self::Buf;

    /// Convert a discriminant into the corresponding variant
    fn from_u8(x: u8) -> Option<Self>;

    /// Convert a variant back into the wire representation
    fn as_bytes(&self) -> &'static [u8];
}

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
                const OFF: usize = $off;
                const MIN: usize = tag::min_of(&[$($TOK.len()),+]);

                type Buf = [u8; tag::buf_of(&[$($TOK.len()),+])];

                #[inline(always)]
                fn make_buf() -> Self::Buf {
                    [0u8; tag::buf_of(&[$($TOK.len()),+])]
                }

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

const fn max_of(mut xs: &[usize]) -> usize {
    let mut y = usize::MIN;
    while let &[x, ref tail @ ..] = xs {
        y = if x > y { x } else { y };
        xs = tail;
    }
    y
}

pub const fn min_of(mut xs: &[usize]) -> usize {
    let mut y = usize::MAX;
    while let &[x, ref tail @ ..] = xs {
        y = if x < y { x } else { y };
        xs = tail;
    }
    y
}

pub const fn buf_of(xs: &[usize]) -> usize {
    max_of(&[min_of(xs), max_of(xs) - min_of(xs)])
}
