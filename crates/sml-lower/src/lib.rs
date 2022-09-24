//! Lowers AST into HIR.

#![deny(missing_debug_implementations, missing_docs, rust_2018_idioms)]

mod common;
mod dec;
mod exp;
mod pat;
mod root;
mod ty;
mod util;

pub use root::get;
pub use util::{Error, Lower, Ptrs};
