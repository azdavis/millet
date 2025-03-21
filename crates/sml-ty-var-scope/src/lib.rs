//! Adding implicitly-scoped type variables to explicit binding sites.
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

use fast_hash::{FxHashMap, FxHashSet};

/// Computes what type variables to add.
pub fn get(ars: &mut sml_hir::Arenas, root: &[sml_hir::StrDecIdx]) {
  let mut st = St::default();
  // it is troublesome to try to actually add the type variables as we traverse, since then the dec
  // arena needs to be mutable, and that causes all sorts of unhappiness since we need to traverse
  // it. traversing and mutating a thing at the same time is not easy.
  //
  // to solve this, we first go over the whole dec and mutate the st, not the arenas.
  get_str_dec(&mut st, ars, root);
  // now we know what we need to do, we just need to do it.
  for (dec, implicit) in st.val_dec {
    let sml_hir::Dec::Val(ty_vars, _, _) = &mut ars.dec[dec] else { unreachable!("Dec not Val") };
    ty_vars.extend(implicit);
  }
  for (spec, implicit) in st.val_spec {
    let sml_hir::Spec::Val(ty_vars, _) = &mut ars.spec[spec] else { unreachable!("Spec not Val") };
    ty_vars.extend(implicit);
  }
}

/// The `val` decs/specs and the type variables they implicitly bind.
#[derive(Debug, Default)]
struct St {
  val_dec: FxHashMap<sml_hir::la_arena::Idx<sml_hir::Dec>, TyVarSet>,
  val_spec: FxHashMap<sml_hir::la_arena::Idx<sml_hir::Spec>, TyVarSet>,
}

type TyVarSet = FxHashSet<sml_hir::TyVar>;

/// this just goes over each single str dec in sequence.
fn get_str_dec(st: &mut St, ars: &sml_hir::Arenas, str_decs: &[sml_hir::StrDecIdx]) {
  for &str_dec in str_decs {
    get_str_dec_one(st, ars, str_dec);
  }
}

fn get_str_dec_one(st: &mut St, ars: &sml_hir::Arenas, str_dec: sml_hir::StrDecIdx) {
  match &ars.str_dec[str_dec] {
    // the only mildly interesting case. we go over the dec twice. first, we get unguarded type
    // variables, then we determine which ones should be bound where.
    sml_hir::StrDec::Dec(dec) => {
      let mut mode = Mode::GetUnguarded(TyVarSet::default());
      get_dec(st, ars, &TyVarSet::default(), &mut mode, dec);
      mode = Mode::FilterBoundAbove;
      get_dec(st, ars, &TyVarSet::default(), &mut mode, dec);
    }
    sml_hir::StrDec::Structure(str_binds) => {
      for str_bind in str_binds {
        get_str_exp(st, ars, str_bind.str_exp);
      }
    }
    sml_hir::StrDec::Signature(sig_binds) => {
      for sig_bind in sig_binds {
        get_sig_exp(st, ars, sig_bind.sig_exp);
      }
    }
    sml_hir::StrDec::Functor(fun_binds) => {
      for fun_bind in fun_binds {
        get_sig_exp(st, ars, fun_bind.param_sig);
        get_str_exp(st, ars, fun_bind.body);
      }
    }
    sml_hir::StrDec::Local(local_dec, in_dec) => {
      get_str_dec(st, ars, local_dec);
      get_str_dec(st, ars, in_dec);
    }
  }
}

fn get_str_exp(st: &mut St, ars: &sml_hir::Arenas, str_exp: sml_hir::StrExpIdx) {
  let Some(str_exp) = str_exp else { return };
  match &ars.str_exp[str_exp] {
    sml_hir::StrExp::Struct(str_dec) => get_str_dec(st, ars, str_dec),
    sml_hir::StrExp::Ascription(str_exp, _, sig_exp) => {
      get_str_exp(st, ars, *str_exp);
      get_sig_exp(st, ars, *sig_exp);
    }
    sml_hir::StrExp::App(_, str_exp, _) => get_str_exp(st, ars, *str_exp),
    sml_hir::StrExp::Let(str_dec, str_exp) => {
      get_str_dec(st, ars, str_dec);
      get_str_exp(st, ars, *str_exp);
    }
    sml_hir::StrExp::Path(_) => {}
  }
}

fn get_sig_exp(st: &mut St, ars: &sml_hir::Arenas, sig_exp: sml_hir::SigExpIdx) {
  let Some(sig_exp) = sig_exp else { return };
  match &ars.sig_exp[sig_exp] {
    sml_hir::SigExp::Spec(spec) => get_spec(st, ars, spec),
    sml_hir::SigExp::Where(sig_exp, _) => get_sig_exp(st, ars, *sig_exp),
    sml_hir::SigExp::Name(_) => {}
  }
}

fn get_spec(st: &mut St, ars: &sml_hir::Arenas, specs: &[sml_hir::SpecIdx]) {
  for &spec in specs {
    get_spec_one(st, ars, spec);
  }
}

fn get_spec_one(st: &mut St, ars: &sml_hir::Arenas, spec: sml_hir::SpecIdx) {
  match &ars.spec[spec] {
    sml_hir::Spec::Val(_, val_descs) => {
      // specs are simpler than val decs. there's no need to do multiple passes, consider nested
      // declarations, or scan expressions or patterns for free ty var occurrences. we need only
      // look at the types on each val desc.
      let mut ac = TyVarSet::default();
      for val_desc in val_descs {
        get_ty(ars, &mut ac, val_desc.ty);
      }
      assert!(st.val_spec.insert(spec, ac).is_none());
    }
    sml_hir::Spec::Str(str_desc) => get_sig_exp(st, ars, str_desc.sig_exp),
    sml_hir::Spec::Include(sig_exp) => get_sig_exp(st, ars, *sig_exp),
    sml_hir::Spec::Sharing(spec, _, _) => get_spec(st, ars, spec),
    sml_hir::Spec::Ty(_)
    | sml_hir::Spec::EqTy(_)
    | sml_hir::Spec::Datatype(_, _)
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
  /// We're getting the unguarded ty vars in this declaration.
  GetUnguarded(TyVarSet),
  /// We finished getting the unguarded ty vars. Now we're re-going over the decs to see
  /// exactly where the ty vars should be bound, by filtering out ty vars that have already been
  /// implicitly bound by a containing val dec.
  FilterBoundAbove,
}

/// this just goes over each single dec in sequence.
fn get_dec(
  st: &mut St,
  ars: &sml_hir::Arenas,
  scope: &TyVarSet,
  mode: &mut Mode,
  decs: &[sml_hir::DecIdx],
) {
  for &dec in decs {
    get_dec_one(st, ars, scope, mode, dec);
  }
}

/// the most interesting fn.
fn get_dec_one(
  st: &mut St,
  ars: &sml_hir::Arenas,
  scope: &TyVarSet,
  mode: &mut Mode,
  dec: sml_hir::DecIdx,
) {
  match &ars.dec[dec] {
    // the most interesting case.
    sml_hir::Dec::Val(ty_vars, val_binds, _) => {
      // `scope` is used in both GetUnguarded and FilterBoundAbove modes, but slightly differently
      // each time.
      //
      // in GetUnguarded mode, `scope` is the set of ty vars for which there is a containing
      // user-written ty var binder that has **explicitly** brought that ty var into scope. .
      //
      // in FilterBoundAbove mode, `scope` is the set of ty vars for which there is a containing
      // `val` decs that we have determined will **implicitly** bring that ty var into scope.
      let mut scope = scope.clone();
      match mode {
        Mode::GetUnguarded(_) => {
          scope.extend(ty_vars.iter().cloned());
          // we use a completely new mode for this val dec, shadowing the original mode passed in
          // from above. this is because we want the GetUnguarded accumulator to contain those type
          // variables which are **unguarded** in this dec.
          //
          // from the definition, paraphrased (p 20):
          //
          // > A free occurrence of a ty var in a val dec is said to be unguarded if the occurrence
          // > is not part of a smaller val dec within that value declaration. In this case we say
          // > that the ty var occurs unguarded in the val dec.
          let mut mode = Mode::GetUnguarded(TyVarSet::default());
          for val_bind in val_binds {
            match &mut mode {
              Mode::GetUnguarded(ac) => get_pat(ars, ac, val_bind.pat),
              Mode::FilterBoundAbove => unreachable!("mode changed"),
            }
            get_exp(st, ars, &scope, &mut mode, val_bind.exp);
          }
          let mut ac = match mode {
            Mode::GetUnguarded(ac) => ac,
            Mode::FilterBoundAbove => unreachable!("mode changed"),
          };
          // we want only free occurrences, so ignore those already explicitly in scope
          for ty_var in &scope {
            ac.remove(ty_var);
          }
          assert!(st.val_dec.insert(dec, ac).is_none());
        }
        Mode::FilterBoundAbove => {
          // from the definition, paraphrased, continuing from the "unguarded" discussion above:
          //
          // > We say that 'a is implicitly scoped at a particular val dec in a program if:
          // >
          // > 1. 'a occurs unguarded in this val dec, and
          // > 2. 'a does not occur unguarded in any larger val dec containing the given one.
          //
          // here we get the ty vars, which are currently exactly the unguarded variables for this
          // dec, fulfilling condition 1.
          let ty_vars = st.val_dec.get_mut(&dec).expect("should have done GetUnguarded first");
          // we then mutate to remove any variables implicitly scoped by any val decs that contain
          // this one, fulfilling condition 2.
          //
          // note that because we used `get_mut`, this updates `st` itself for later.
          for ty_var in &scope {
            ty_vars.remove(ty_var);
          }
          // we note that these ty vars are now (implicitly) bound, so we don't re-bind them in any
          // val decs contained in this one.
          scope.extend(ty_vars.iter().cloned());
          // finally, we recur.
          for val_bind in val_binds {
            get_exp(st, ars, &scope, mode, val_bind.exp);
          }
        }
      }
    }
    sml_hir::Dec::Local(local_dec, in_dec) => {
      get_dec(st, ars, scope, mode, local_dec);
      get_dec(st, ars, scope, mode, in_dec);
    }
    sml_hir::Dec::Exception(ex_binds) => match mode {
      Mode::GetUnguarded(ac) => {
        for ex_bind in ex_binds {
          match *ex_bind {
            sml_hir::ExBind::New(_, Some(ty)) => get_ty(ars, ac, ty),
            sml_hir::ExBind::New(..) | sml_hir::ExBind::Copy(..) => {}
          }
        }
      }
      Mode::FilterBoundAbove => {}
    },
    sml_hir::Dec::Ty(ty_binds) => match mode {
      Mode::GetUnguarded(ac) => get_ty_binds(ars, ac, ty_binds),
      Mode::FilterBoundAbove => {}
    },
    sml_hir::Dec::Datatype(dat_binds, ty_binds) => match mode {
      Mode::GetUnguarded(ac) => {
        get_ty_binds(ars, ac, ty_binds);
        for dat_bind in dat_binds {
          let mut dat_ac = TyVarSet::default();
          for con_bind in &dat_bind.cons {
            if let Some(ty) = con_bind.ty {
              get_ty(ars, &mut dat_ac, ty);
            }
          }
          for ty_var in &dat_bind.ty_vars {
            dat_ac.remove(ty_var);
          }
          ac.extend(dat_ac);
        }
      }
      Mode::FilterBoundAbove => {}
    },
    // Abstype is not implemented. DatatypeCopy and Open contain only names/paths.
    sml_hir::Dec::Abstype(..) | sml_hir::Dec::DatatypeCopy(..) | sml_hir::Dec::Open(..) => {}
  }
}

fn get_ty_binds(ars: &sml_hir::Arenas, ac: &mut TyVarSet, ty_binds: &[sml_hir::TyBind]) {
  for ty_bind in ty_binds {
    let mut ty_ac = TyVarSet::default();
    get_ty(ars, &mut ty_ac, ty_bind.ty);
    for ty_var in &ty_bind.ty_vars {
      ty_ac.remove(ty_var);
    }
    ac.extend(ty_ac);
  }
}

/// mostly passes the `mode` down to further fns that need it. also uses the mode to explore further
/// parts of the expression in `GetUnguarded` mode, but ignores those parts in `FilterBoundAbove`
/// mode.
fn get_exp(
  st: &mut St,
  ars: &sml_hir::Arenas,
  scope: &TyVarSet,
  mode: &mut Mode,
  exp: sml_hir::ExpIdx,
) {
  let Some(exp) = exp else { return };
  match &ars.exp[exp] {
    sml_hir::Exp::Record(rows) => {
      for &(_, exp) in rows {
        get_exp(st, ars, scope, mode, exp);
      }
    }
    sml_hir::Exp::Let(dec, exp) => {
      get_dec(st, ars, scope, mode, dec);
      get_exp(st, ars, scope, mode, *exp);
    }
    sml_hir::Exp::App(func, argument) => {
      get_exp(st, ars, scope, mode, *func);
      get_exp(st, ars, scope, mode, *argument);
    }
    sml_hir::Exp::Handle(head, matcher_arms) => {
      get_exp(st, ars, scope, mode, *head);
      for arm in matcher_arms {
        match mode {
          Mode::GetUnguarded(ac) => get_pat(ars, ac, arm.pat),
          Mode::FilterBoundAbove => {}
        }
        get_exp(st, ars, scope, mode, arm.exp);
      }
    }
    sml_hir::Exp::Raise(exp) => get_exp(st, ars, scope, mode, *exp),
    sml_hir::Exp::Fn(matcher_arms, _) => {
      for arm in matcher_arms {
        match mode {
          Mode::GetUnguarded(ac) => get_pat(ars, ac, arm.pat),
          Mode::FilterBoundAbove => {}
        }
        get_exp(st, ars, scope, mode, arm.exp);
      }
    }
    sml_hir::Exp::Typed(exp, ty, _) => {
      get_exp(st, ars, scope, mode, *exp);
      match mode {
        Mode::GetUnguarded(ac) => get_ty(ars, ac, *ty),
        Mode::FilterBoundAbove => {}
      }
    }
    sml_hir::Exp::Vector(exps) => {
      for exp in exps {
        get_exp(st, ars, scope, mode, *exp);
      }
    }
    sml_hir::Exp::Hole | sml_hir::Exp::SCon(_) | sml_hir::Exp::Path(_) => {}
  }
}

/// records all encountered type variables in `ac`.
fn get_pat(ars: &sml_hir::Arenas, ac: &mut TyVarSet, pat: sml_hir::PatIdx) {
  let Some(pat) = pat else { return };
  match &ars.pat[pat] {
    sml_hir::Pat::Con(_, argument) => get_pat(ars, ac, argument.flatten()),
    sml_hir::Pat::Record { rows, .. } => {
      for &(_, pat) in rows {
        get_pat(ars, ac, pat);
      }
    }
    sml_hir::Pat::Typed(pat, ty) => {
      get_pat(ars, ac, *pat);
      get_ty(ars, ac, *ty);
    }
    sml_hir::Pat::As(_, pat) => get_pat(ars, ac, *pat),
    sml_hir::Pat::Or(or_pat) => {
      for pat in or_pat.all_pats() {
        get_pat(ars, ac, pat);
      }
    }
    sml_hir::Pat::Vector(pats) => {
      for &pat in pats {
        get_pat(ars, ac, pat);
      }
    }
    sml_hir::Pat::Wild | sml_hir::Pat::SCon(_) => {}
  }
}

/// records all encountered type variables in `ac`.
fn get_ty(ars: &sml_hir::Arenas, ac: &mut TyVarSet, ty: sml_hir::TyIdx) {
  let Some(ty) = ty else { return };
  match &ars.ty[ty] {
    // the only slightly interesting case, where we note the ty variable we saw. the other cases are
    // just any applicable recursion.
    sml_hir::Ty::Var(tv) => {
      ac.insert(tv.clone());
    }
    sml_hir::Ty::Record(rows) => {
      for &(_, ty) in rows {
        get_ty(ars, ac, ty);
      }
    }
    sml_hir::Ty::Con(arguments, _) => {
      for &ty in arguments {
        get_ty(ars, ac, ty);
      }
    }
    sml_hir::Ty::Fn(param, res) => {
      get_ty(ars, ac, *param);
      get_ty(ars, ac, *res);
    }
    sml_hir::Ty::Hole => {}
  }
}
