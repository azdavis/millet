//! Lowers AST into HIR.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]
#![allow(unused)]

mod common;
mod dec;
mod exp;
mod pat;
mod root;
mod top_dec;
mod ty;
mod util;

pub use root::get;
pub use util::{Lowered, Ptrs};
