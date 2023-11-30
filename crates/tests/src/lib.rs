//! Tests.
//!
//! - If you're not sure where to put a test, put it in misc.
//! - If you have many similar tests, put them in an existing or new module.

#![cfg(test)]
#![deny(clippy::pedantic, rust_2018_idioms)]
#![allow(clippy::single_match_else)]

mod big;
mod cannot_rebind;
mod check;
mod circularity;
mod common;
mod completions;
mod datatype_copy;
mod deviations;
mod disallow;
mod docs;
mod dupe;
mod empty;
mod equality;
mod exn;
mod fixity;
mod forbid_opaque_asc;
mod functor;
mod generalize;
mod goto_def;
mod hover;
mod incomplete;
mod infix_without_op;
mod input;
mod literal;
mod local;
mod matching;
mod misc;
mod num_record;
mod open;
mod overload;
mod pat;
mod repo;
mod rest_pat;
mod rust;
mod sep;
mod shadow;
mod sig;
mod sig_fun_file;
mod smoke;
mod std_basis;
mod symbolic;
mod ty_escape;
mod ty_var;
mod unused;
mod use_builtin;
mod val_rec;
mod well_known;
