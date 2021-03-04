//! Types for working with SML syntax trees.

#![deny(rust_2018_idioms)]

pub mod ast;
mod kind;

pub use event_parse;
pub use kind::*;
pub use rowan;
