//! Lowering AST into HIR.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
#![allow(clippy::too_many_lines)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]

mod common;
mod dec;
mod exp;
mod pat;
mod root;
mod ty;
mod util;

pub use root::get;
pub use util::{Error, Lower, Ptrs};
