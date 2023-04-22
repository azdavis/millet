//! The dynamic semantics, aka, running a program.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]

mod display;
mod dynamics;
mod pat_match;
mod step;
mod types;

pub use dynamics::Dynamics;
pub use types::Cx;
