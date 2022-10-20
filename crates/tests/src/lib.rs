//! Tests.
//!
//! - If you're not sure where to put a test, put it in misc.
//! - If you have many similar tests, put them in an existing or new module.

#![cfg(test)]
#![deny(clippy::pedantic, rust_2018_idioms)]

mod check;
mod datatype_copy;
mod deviations;
mod docs;
mod dupe;
mod exn;
mod fixity;
mod functor;
mod generalize;
mod hover;
mod incomplete;
mod infix_without_op;
mod input;
mod literal;
mod local;
mod matching;
mod misc;
mod num_record;
mod overload;
mod pat;
mod repo;
mod rest_pat;
mod rust;
mod shadow;
mod sig;
mod smoke;
mod std_basis;
mod ty_name_escape;
mod ty_var;
