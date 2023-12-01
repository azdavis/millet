//! Lowering AST into HIR.

#![allow(clippy::too_many_lines, clippy::single_match_else)]

mod common;
mod dec;
mod exp;
mod pat;
mod root;
mod ty;
mod util;

pub use root::get;
pub use util::{Error, Lower, Ptrs};
