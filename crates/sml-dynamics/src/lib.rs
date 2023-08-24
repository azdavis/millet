//! The dynamic semantics, aka, running a program.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

mod display;
mod dynamics;
mod pat_match;
mod step;
mod types;

pub use dynamics::{Dynamics, Progress};
pub use types::Cx;
