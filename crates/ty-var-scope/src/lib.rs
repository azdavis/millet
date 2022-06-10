//! Add implicitly-scoped type variables to explicit binding sites.
//!
//! ```sml
//! type 'a guy = unit
//! val _ =
//! (* ^ 'a bound here *)
//!   let
//!     val _ = (): 'a guy
//!   in
//!     (): 'a guy
//!   end
//!
//! val _ =
//!   let
//!     val _ = (): 'a guy
//! (*    ^ 'a bound here *)
//!   in
//!     ()
//!   end
//!
//! val _ =
//! (* ^ 'a bound here *)
//!   let
//!     val _ = ()
//!   in
//!     (): 'a guy
//!   end
//! ```

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

use fast_hash::{FxHashMap, FxHashSet};

/// The `val` decs and the type variables they implicitly bind.
pub type Cx = FxHashMap<hir::la_arena::Idx<hir::Dec>, Vec<hir::TyVar>>;

/// Computes what type variables to add.
///
/// It is somewhat troublesome to try to actually add the type variables as we traverse, since then
/// the dec arena needs to be mutable, and that causes all sorts of unhappiness since we need to
/// traverse it. Traversing and mutating a thing at the same time is not easy.
pub fn get(ars: &hir::Arenas, top_decs: &[hir::TopDecIdx]) -> Cx {
  let mut cx = Cx::default();
  for &top_dec in top_decs {
    get_top_dec(&mut cx, ars, top_dec);
  }
  cx
}

type TyVarSet = FxHashSet<hir::TyVar>;

fn get_top_dec(cx: &mut Cx, ars: &hir::Arenas, top_dec: hir::TopDecIdx) {
  match &ars.top_dec[top_dec] {
    hir::TopDec::Str(str_dec) => get_str_dec(cx, ars, *str_dec),
    hir::TopDec::Sig(_) => {}
    hir::TopDec::Functor(fun_binds) => {
      for fun_bind in fun_binds {
        get_str_exp(cx, ars, fun_bind.body)
      }
    }
  }
}

fn get_str_dec(cx: &mut Cx, ars: &hir::Arenas, str_dec: hir::StrDecIdx) {
  let str_dec = match str_dec {
    Some(x) => x,
    None => return,
  };
  match &ars.str_dec[str_dec] {
    hir::StrDec::Dec(dec) => get_dec(cx, ars, &TyVarSet::default(), *dec),
    hir::StrDec::Structure(str_binds) => {
      for str_bind in str_binds {
        get_str_exp(cx, ars, str_bind.str_exp);
      }
    }
    hir::StrDec::Local(local_dec, in_dec) => {
      get_str_dec(cx, ars, *local_dec);
      get_str_dec(cx, ars, *in_dec);
    }
    hir::StrDec::Seq(str_decs) => {
      for &str_dec in str_decs {
        get_str_dec(cx, ars, str_dec);
      }
    }
  }
}

fn get_str_exp(cx: &mut Cx, ars: &hir::Arenas, str_exp: hir::StrExpIdx) {
  let str_exp = match str_exp {
    Some(x) => x,
    None => return,
  };
  match &ars.str_exp[str_exp] {
    hir::StrExp::Struct(str_dec) => get_str_dec(cx, ars, *str_dec),
    hir::StrExp::Path(_) => {}
    hir::StrExp::Ascription(str_exp, _, _) => get_str_exp(cx, ars, *str_exp),
    hir::StrExp::App(_, str_exp) => get_str_exp(cx, ars, *str_exp),
    hir::StrExp::Let(str_dec, str_exp) => {
      get_str_dec(cx, ars, *str_dec);
      get_str_exp(cx, ars, *str_exp);
    }
  }
}

/// `scope` is already bound variables.
fn get_dec(cx: &mut Cx, ars: &hir::Arenas, scope: &TyVarSet, dec: hir::DecIdx) {
  let dec = match dec {
    Some(x) => x,
    None => return,
  };
  match &ars.dec[dec] {
    hir::Dec::Val(ty_vars, val_binds) => {
      let mut scope = scope.clone();
      scope.extend(ty_vars.iter().cloned());
      let mut ac = TyVarSet::default();
      for val_bind in val_binds {
        get_pat(cx, ars, &mut ac, val_bind.pat);
        get_exp(cx, ars, &scope, &mut ac, val_bind.exp);
      }
      let mut to_bind: Vec<_> = ac.difference(&scope).cloned().collect();
      to_bind.sort_unstable();
      cx.insert(dec, to_bind);
    }
    hir::Dec::Abstype(_, dec) => get_dec(cx, ars, scope, *dec),
    hir::Dec::Local(local_dec, in_dec) => {
      get_dec(cx, ars, scope, *local_dec);
      get_dec(cx, ars, scope, *in_dec);
    }
    hir::Dec::Seq(decs) => {
      for &dec in decs {
        get_dec(cx, ars, scope, dec);
      }
    }
    hir::Dec::Ty(_)
    | hir::Dec::Datatype(_)
    | hir::Dec::DatatypeCopy(_, _)
    | hir::Dec::Exception(_)
    | hir::Dec::Open(_) => {}
  }
}

fn get_exp(cx: &mut Cx, ars: &hir::Arenas, scope: &TyVarSet, ac: &mut TyVarSet, exp: hir::ExpIdx) {
  let exp = match exp {
    Some(x) => x,
    None => return,
  };
  match &ars.exp[exp] {
    hir::Exp::SCon(_) | hir::Exp::Path(_) => {}
    hir::Exp::Record(rows) => {
      for &(_, exp) in rows {
        get_exp(cx, ars, scope, ac, exp);
      }
    }
    hir::Exp::Let(dec, exp) => {
      get_dec(cx, ars, scope, *dec);
      get_exp(cx, ars, scope, ac, *exp);
    }
    hir::Exp::App(func, arg) => {
      get_exp(cx, ars, scope, ac, *func);
      get_exp(cx, ars, scope, ac, *arg);
    }
    hir::Exp::Handle(head, arms) => {
      get_exp(cx, ars, scope, ac, *head);
      for &(pat, exp) in arms {
        get_pat(cx, ars, ac, pat);
        get_exp(cx, ars, scope, ac, exp);
      }
    }
    hir::Exp::Raise(exp) => get_exp(cx, ars, scope, ac, *exp),
    hir::Exp::Fn(arms) => {
      for &(pat, exp) in arms {
        get_pat(cx, ars, ac, pat);
        get_exp(cx, ars, scope, ac, exp);
      }
    }
    hir::Exp::Typed(exp, ty) => {
      get_exp(cx, ars, scope, ac, *exp);
      get_ty(cx, ars, ac, *ty);
    }
  }
}

fn get_pat(cx: &mut Cx, ars: &hir::Arenas, ac: &mut TyVarSet, pat: hir::PatIdx) {
  let pat = match pat {
    Some(x) => x,
    None => return,
  };
  match &ars.pat[pat] {
    hir::Pat::Wild | hir::Pat::SCon(_) => {}
    hir::Pat::Con(_, arg) => get_pat(cx, ars, ac, arg.flatten()),
    hir::Pat::Record { rows, .. } => {
      for &(_, pat) in rows {
        get_pat(cx, ars, ac, pat)
      }
    }
    hir::Pat::Typed(pat, ty) => {
      get_pat(cx, ars, ac, *pat);
      get_ty(cx, ars, ac, *ty);
    }
    hir::Pat::As(_, pat) => get_pat(cx, ars, ac, *pat),
  }
}

fn get_ty(cx: &mut Cx, ars: &hir::Arenas, ac: &mut TyVarSet, ty: hir::TyIdx) {
  let ty = match ty {
    Some(x) => x,
    None => return,
  };
  match &ars.ty[ty] {
    hir::Ty::Var(tv) => {
      ac.insert(tv.clone());
    }
    hir::Ty::Record(rows) => {
      for &(_, ty) in rows {
        get_ty(cx, ars, ac, ty);
      }
    }
    hir::Ty::Con(args, _) => {
      for &ty in args {
        get_ty(cx, ars, ac, ty);
      }
    }
    hir::Ty::Fn(param, res) => {
      get_ty(cx, ars, ac, *param);
      get_ty(cx, ars, ac, *res);
    }
  }
}
