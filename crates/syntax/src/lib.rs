//! Types for working with SML syntax trees.

#![deny(rust_2018_idioms)]

pub mod ast {
  include!(concat!(env!("OUT_DIR"), "/ast.rs"));
}
mod kind {
  include!(concat!(env!("OUT_DIR"), "/kind.rs"));
}

pub use kind::*;
pub use rowan;
pub use token;
