//! Contains builtins that fetch paths from the Internet

use tvix_eval::builtin_macros::builtins;

#[builtins(state = "Rc<TvixStoreIO>")]
pub(crate) mod fetcher_builtins {}
