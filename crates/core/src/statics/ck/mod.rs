//! Check ASTs for validity.
//!
//! We pass around one big `Subst` in the `State`. This `Subst` is constantly mutably updated as we
//! discover more types that must unify. However, note that `Subst#unify` accepts the two `Ty`s by
//! move, and so does not update the types themselves if and when the call to `unify` has
//! successfully updated the `Subst`. This means that _if_ you want to have the types be updated as
//! well, you must call `Ty#apply` with the new `Subst`.
//!
//! It is only strictly necessary to call `Ty#apply` in the following situations:
//!
//! 1. Right before returning from `instantiate`. This is a bit of a special case because the
//!    `Subst` there is just a one-off which maps old type variables to new type variables, and is
//!    not part of the overall global `Subst`.
//! 2. Right before calling `exhaustive::ck`. This is so that we may correctly compute the
//!    obligations the patterns need to fulfil.
//! 3. Right before calling `generalize`. This is so that we know exactly what type variables ought
//!    to be generalized and which have already been solved to concrete types by the `Subst`.
//! 4. Right before checking for type name escape. This is so that we know exactly what type names
//!    are mentioned in the type we are checking.
//!
//! In short, we only need to call `apply` when we truly need access to _everything_ we currently
//! know about this type. In many situations, for instance, we don't need to call `apply` on a fresh
//! type variable we are about to return from `ck_exp` (e.g. the App case) because we have already
//! recorded the relevant information in the `Subst` and this information will be surfaced later by
//! one of the above noted places that we do call `apply`.

mod dec;
mod pat;
mod ty;
mod util;

use crate::ast::{StrDec, TopDec};
use crate::intern::StrRef;
use crate::loc::Located;
use crate::statics::types::{Basis, Cx, Result, State, StaticsError, TyVarSet};

pub fn ck_top_dec(bs: Basis, st: &mut State, top_dec: &Located<TopDec<StrRef>>) -> Result<Basis> {
  match &top_dec.val {
    TopDec::StrDec(str_dec) => match &str_dec.val {
      StrDec::Dec(dec) => {
        let mut cx = Cx {
          ty_names: bs.ty_names,
          ty_vars: TyVarSet::new(),
          env: bs.env,
        };
        cx.o_plus(dec::ck(&cx, st, dec)?);
        Ok(Basis {
          env: cx.env,
          ty_names: cx.ty_names,
          ..bs
        })
      }
      StrDec::Structure(_) => Err(top_dec.loc.wrap(StaticsError::Todo)),
      StrDec::Local(_, _) => Err(top_dec.loc.wrap(StaticsError::Todo)),
      StrDec::Seq(_) => Err(top_dec.loc.wrap(StaticsError::Todo)),
    },
    TopDec::SigDec(_) => Err(top_dec.loc.wrap(StaticsError::Todo)),
    TopDec::FunDec(_) => Err(top_dec.loc.wrap(StaticsError::Todo)),
  }
}
