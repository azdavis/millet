//! Add implicitly-scoped type variables to explicit binding sites.
//!
//! ```sml
//! type 'a guy = unit
//!
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
//!     val _ = (): 'b guy
//! (*    ^ 'b bound here *)
//!   in
//!     ()
//!   end
//!
//! val _ =
//! (* ^ 'c bound here *)
//!   let
//!     val _ = ()
//!   in
//!     (): 'c guy
//!   end
//! ```

#![deny(missing_debug_implementations, missing_docs, rust_2018_idioms)]

use fast_hash::{FxHashMap, FxHashSet};

/// Computes what type variables to add.
///
/// It is somewhat troublesome to try to actually add the type variables as we traverse, since then
/// the dec arena needs to be mutable, and that causes all sorts of unhappiness since we need to
/// traverse it. Traversing and mutating a thing at the same time is not easy.
pub fn get(ars: &mut sml_hir::Arenas, root: sml_hir::StrDecIdx) {
  let mut cx = Cx::default();
  get_str_dec(&mut cx, ars, root);
  for (dec, implicit) in cx.val_dec {
    match &mut ars.dec[dec] {
      sml_hir::Dec::Val(ty_vars, _) => ty_vars.extend(implicit),
      _ => unreachable!("only val may implicitly bind ty vars"),
    }
  }
  for (spec, implicit) in cx.val_spec {
    match &mut ars.spec[spec] {
      sml_hir::Spec::Val(ty_vars, _) => ty_vars.extend(implicit),
      _ => unreachable!("only val may implicitly bind ty vars"),
    }
  }
}

/// The `val` decs/specs and the type variables they implicitly bind.
#[derive(Debug, Default)]
struct Cx {
  val_dec: FxHashMap<sml_hir::la_arena::Idx<sml_hir::Dec>, TyVarSet>,
  val_spec: FxHashMap<sml_hir::la_arena::Idx<sml_hir::Spec>, TyVarSet>,
}

type TyVarSet = FxHashSet<sml_hir::TyVar>;

fn get_str_dec(cx: &mut Cx, ars: &sml_hir::Arenas, str_dec: sml_hir::StrDecIdx) {
  let str_dec = match str_dec {
    Some(x) => x,
    None => return,
  };
  match &ars.str_dec[str_dec] {
    sml_hir::StrDec::Dec(dec) => {
      let mut mode = Mode::Get(TyVarSet::default());
      get_dec(cx, ars, &TyVarSet::default(), &mut mode, *dec);
      mode = Mode::Set;
      get_dec(cx, ars, &TyVarSet::default(), &mut mode, *dec);
    }
    sml_hir::StrDec::Structure(str_binds) => {
      for str_bind in str_binds {
        get_str_exp(cx, ars, str_bind.str_exp);
      }
    }
    sml_hir::StrDec::Local(local_dec, in_dec) => {
      get_str_dec(cx, ars, *local_dec);
      get_str_dec(cx, ars, *in_dec);
    }
    sml_hir::StrDec::Seq(str_decs) => {
      for &str_dec in str_decs {
        get_str_dec(cx, ars, str_dec);
      }
    }
    sml_hir::StrDec::Signature(sig_binds) => {
      for sig_bind in sig_binds {
        get_sig_exp(cx, ars, sig_bind.sig_exp);
      }
    }
    sml_hir::StrDec::Functor(fun_binds) => {
      for fun_bind in fun_binds {
        get_sig_exp(cx, ars, fun_bind.param_sig);
        get_str_exp(cx, ars, fun_bind.body);
      }
    }
  }
}

fn get_str_exp(cx: &mut Cx, ars: &sml_hir::Arenas, str_exp: sml_hir::StrExpIdx) {
  let str_exp = match str_exp {
    Some(x) => x,
    None => return,
  };
  match &ars.str_exp[str_exp] {
    sml_hir::StrExp::Struct(str_dec) => get_str_dec(cx, ars, *str_dec),
    sml_hir::StrExp::Path(_) => {}
    sml_hir::StrExp::Ascription(str_exp, _, sig_exp) => {
      get_str_exp(cx, ars, *str_exp);
      get_sig_exp(cx, ars, *sig_exp);
    }
    sml_hir::StrExp::App(_, str_exp) => get_str_exp(cx, ars, *str_exp),
    sml_hir::StrExp::Let(str_dec, str_exp) => {
      get_str_dec(cx, ars, *str_dec);
      get_str_exp(cx, ars, *str_exp);
    }
  }
}

fn get_sig_exp(cx: &mut Cx, ars: &sml_hir::Arenas, sig_exp: sml_hir::SigExpIdx) {
  let sig_exp = match sig_exp {
    Some(x) => x,
    None => return,
  };
  match &ars.sig_exp[sig_exp] {
    sml_hir::SigExp::Spec(spec) => get_spec(cx, ars, *spec),
    sml_hir::SigExp::Name(_) => {}
    sml_hir::SigExp::WhereType(sig_exp, _, _, _) | sml_hir::SigExp::Where(sig_exp, _, _) => {
      get_sig_exp(cx, ars, *sig_exp)
    }
  }
}

fn get_spec(cx: &mut Cx, ars: &sml_hir::Arenas, spec: sml_hir::SpecIdx) {
  let spec = match spec {
    Some(x) => x,
    None => return,
  };
  match &ars.spec[spec] {
    sml_hir::Spec::Val(_, val_descs) => {
      let mut ac = TyVarSet::default();
      for val_desc in val_descs {
        get_ty(cx, ars, &mut ac, val_desc.ty);
      }
      assert!(cx.val_spec.insert(spec, ac).is_none());
    }
    sml_hir::Spec::Str(str_desc) => get_sig_exp(cx, ars, str_desc.sig_exp),
    sml_hir::Spec::Include(sig_exp) => get_sig_exp(cx, ars, *sig_exp),
    sml_hir::Spec::Sharing(spec, _, _) => get_spec(cx, ars, *spec),
    sml_hir::Spec::Seq(specs) => {
      for &spec in specs {
        get_spec(cx, ars, spec);
      }
    }
    sml_hir::Spec::Ty(_)
    | sml_hir::Spec::EqTy(_)
    | sml_hir::Spec::Datatype(_)
    | sml_hir::Spec::DatatypeCopy(_, _)
    | sml_hir::Spec::Exception(_) => {}
  }
}

/// What mode we're in for scoping the type variables at `val` declarations.
///
/// Consider:
///
/// ```sml
/// val x = let val y : 'a list = [] in ... end
/// ```
///
/// We can't know whether `'a` should be bound at `val y` or `val x` without inspecting all of
/// `...`. So, we have to do a first pass to collect all the unguarded ty vars, then a second to
/// scope them and set them.
///
/// Reifying the mode not is probably not the most efficient way to do this, since then we have to
/// do a lot of runtime branching on the mode.
enum Mode {
  /// We're getting the type variables. The set is the set of all type variables in this `val` dec.
  /// (We'll filter out the ones already explicitly in scope later.)
  Get(TyVarSet),
  /// We finished getting the type variables, and now we're setting them to binding sites.
  Set,
}

/// `scope` is already bound variables.
fn get_dec(
  cx: &mut Cx,
  ars: &sml_hir::Arenas,
  scope: &TyVarSet,
  mode: &mut Mode,
  dec: sml_hir::DecIdx,
) {
  let dec = match dec {
    Some(x) => x,
    None => return,
  };
  match &ars.dec[dec] {
    sml_hir::Dec::Val(ty_vars, val_binds) => {
      let mut scope = scope.clone();
      match mode {
        Mode::Get(_) => {
          scope.extend(ty_vars.iter().cloned());
          let mut mode = Mode::Get(TyVarSet::default());
          for val_bind in val_binds {
            match &mut mode {
              Mode::Get(ac) => get_pat(cx, ars, ac, val_bind.pat),
              Mode::Set => unreachable!(),
            }
            get_exp(cx, ars, &scope, &mut mode, val_bind.exp);
          }
          let mut ac = match mode {
            Mode::Get(ac) => ac,
            Mode::Set => unreachable!(),
          };
          for x in scope.iter() {
            ac.remove(x);
          }
          assert!(cx.val_dec.insert(dec, ac).is_none());
        }
        Mode::Set => {
          let unguarded = cx
            .val_dec
            .get_mut(&dec)
            .expect("should have been set in the Get pass");
          for x in scope.iter() {
            unguarded.remove(x);
          }
          scope.extend(unguarded.iter().cloned());
          for val_bind in val_binds {
            get_exp(cx, ars, &scope, mode, val_bind.exp);
          }
        }
      }
    }
    sml_hir::Dec::Abstype(_, _, dec) => get_dec(cx, ars, scope, mode, *dec),
    sml_hir::Dec::Local(local_dec, in_dec) => {
      get_dec(cx, ars, scope, mode, *local_dec);
      get_dec(cx, ars, scope, mode, *in_dec);
    }
    sml_hir::Dec::Seq(decs) => {
      for &dec in decs {
        get_dec(cx, ars, scope, mode, dec);
      }
    }
    sml_hir::Dec::Hole
    | sml_hir::Dec::Ty(_)
    | sml_hir::Dec::Datatype(_, _)
    | sml_hir::Dec::DatatypeCopy(_, _)
    | sml_hir::Dec::Exception(_)
    | sml_hir::Dec::Open(_) => {}
  }
}

fn get_exp(
  cx: &mut Cx,
  ars: &sml_hir::Arenas,
  scope: &TyVarSet,
  mode: &mut Mode,
  exp: sml_hir::ExpIdx,
) {
  let exp = match exp {
    Some(x) => x,
    None => return,
  };
  match &ars.exp[exp] {
    sml_hir::Exp::Hole | sml_hir::Exp::SCon(_) | sml_hir::Exp::Path(_) => {}
    sml_hir::Exp::Record(rows) => {
      for &(_, exp) in rows {
        get_exp(cx, ars, scope, mode, exp);
      }
    }
    sml_hir::Exp::Let(dec, exp) => {
      get_dec(cx, ars, scope, mode, *dec);
      get_exp(cx, ars, scope, mode, *exp);
    }
    sml_hir::Exp::App(func, arg) => {
      get_exp(cx, ars, scope, mode, *func);
      get_exp(cx, ars, scope, mode, *arg);
    }
    sml_hir::Exp::Handle(head, arms) => {
      get_exp(cx, ars, scope, mode, *head);
      for &(pat, exp) in arms {
        match mode {
          Mode::Get(ac) => get_pat(cx, ars, ac, pat),
          Mode::Set => {}
        }
        get_exp(cx, ars, scope, mode, exp);
      }
    }
    sml_hir::Exp::Raise(exp) => get_exp(cx, ars, scope, mode, *exp),
    sml_hir::Exp::Fn(arms) => {
      for &(pat, exp) in arms {
        match mode {
          Mode::Get(ac) => get_pat(cx, ars, ac, pat),
          Mode::Set => {}
        }
        get_exp(cx, ars, scope, mode, exp);
      }
    }
    sml_hir::Exp::Typed(exp, ty) => {
      get_exp(cx, ars, scope, mode, *exp);
      match mode {
        Mode::Get(ac) => get_ty(cx, ars, ac, *ty),
        Mode::Set => {}
      }
    }
  }
}

fn get_pat(cx: &mut Cx, ars: &sml_hir::Arenas, ac: &mut TyVarSet, pat: sml_hir::PatIdx) {
  let pat = match pat {
    Some(x) => x,
    None => return,
  };
  match &ars.pat[pat] {
    sml_hir::Pat::Wild | sml_hir::Pat::SCon(_) => {}
    sml_hir::Pat::Con(_, arg) => get_pat(cx, ars, ac, arg.flatten()),
    sml_hir::Pat::Record { rows, .. } => {
      for &(_, pat) in rows {
        get_pat(cx, ars, ac, pat)
      }
    }
    sml_hir::Pat::Typed(pat, ty) => {
      get_pat(cx, ars, ac, *pat);
      get_ty(cx, ars, ac, *ty);
    }
    sml_hir::Pat::As(_, pat) => get_pat(cx, ars, ac, *pat),
    sml_hir::Pat::Or(or_pat) => {
      get_pat(cx, ars, ac, or_pat.first);
      for &pat in or_pat.rest.iter() {
        get_pat(cx, ars, ac, pat);
      }
    }
  }
}

fn get_ty(cx: &mut Cx, ars: &sml_hir::Arenas, ac: &mut TyVarSet, ty: sml_hir::TyIdx) {
  let ty = match ty {
    Some(x) => x,
    None => return,
  };
  match &ars.ty[ty] {
    sml_hir::Ty::Hole => {}
    sml_hir::Ty::Var(tv) => {
      ac.insert(tv.clone());
    }
    sml_hir::Ty::Record(rows) => {
      for &(_, ty) in rows {
        get_ty(cx, ars, ac, ty);
      }
    }
    sml_hir::Ty::Con(args, _) => {
      for &ty in args {
        get_ty(cx, ars, ac, ty);
      }
    }
    sml_hir::Ty::Fn(param, res) => {
      get_ty(cx, ars, ac, *param);
      get_ty(cx, ars, ac, *res);
    }
  }
}
