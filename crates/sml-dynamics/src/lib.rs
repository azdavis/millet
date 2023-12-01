//! The dynamic semantics, aka, running a program.

mod display;
mod dynamics;
mod pat_match;
mod step;
mod types;

pub use dynamics::{Dynamics, Progress};
pub use types::Cx;
