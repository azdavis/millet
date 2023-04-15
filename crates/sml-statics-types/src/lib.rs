//! Types and fundamental operations for static analysis.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
#![allow(clippy::too_many_lines, clippy::single_match_else)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]

pub mod data;
pub mod def;
pub mod disallow;
pub mod display;
pub mod equality;
pub mod fmt_util;
pub mod generalize;
pub mod info;
pub mod item;
pub mod mode;
pub mod overload;
pub mod sym;
pub mod ty;
pub mod unify;
pub mod util;
