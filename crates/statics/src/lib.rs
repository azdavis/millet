//! Static analysis.
//!
//! With help from [this article][1].
//!
//! [1]: http://dev.stephendiehl.com/fun/006_hindley_milner.html

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]
#![allow(dead_code)]

mod dec;
mod error;
mod exp;
mod pat;
mod pat_match;
mod st;
mod ty;
mod types;
mod unify;
mod util;
