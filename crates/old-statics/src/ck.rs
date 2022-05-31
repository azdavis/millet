//! Check ASTs for validity.
//!
//! We pass around one big `Subst` in the `State`. This `Subst` is constantly mutably updated as we
//! discover more types that must unify. However, note that `Subst#unify` accepts the two `Ty`s by
//! move, and so does not update the types themselves if and when the call to `unify` has
//! successfully updated the `Subst`. This means that _if_ you want to have the types be updated as
//! well, you must call `Ty#apply` with the new `Subst`.
//!
//! It is only strictly necessary to call `Ty#apply` when we truly need access to _everything_ we
//! currently know about this type. In many situations, for instance, we don't need to call `apply`
//! on a fresh type variable we are about to return from `ck_exp` (e.g. the App case) because we
//! have already recorded the relevant information in the `Subst` and this information will be
//! surfaced later by one of the above noted places that we do call `apply`.

mod dec;
mod enrich;
mod exhaustive;
mod pat;
mod sig_match;
mod top_dec;
mod ty;
mod util;

pub(crate) use top_dec::ck as ck_top_dec;
