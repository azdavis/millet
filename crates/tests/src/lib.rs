//! Tests.
//!
//! - If you're not sure where to put a test, put it in misc.
//! - If you have many similar tests, put them in an existing or new module.

#![cfg(test)]
#![deny(rust_2018_idioms)]

mod ascribe;
mod check;
mod datatype_copy;
mod dupe;
mod exn;
mod fixity;
mod functor;
mod incomplete;
mod infix_without_op;
mod literal;
mod local;
mod matching;
mod misc;
mod nj_deviations;
mod num_record;
mod overload;
mod pat;
mod rust;
mod shadow;
mod smoke;
mod std_basis;
mod ty_name_escape;
mod ty_var;
