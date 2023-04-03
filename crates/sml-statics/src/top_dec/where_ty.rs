//! Dealing with `where` and `where type`.

use crate::env::Env;
use crate::top_dec::{realize, ty_con_paths};
use crate::types::{SymsMarker, Ty, TyScheme};
use crate::{
  basis::Bs, dec::add_fixed_ty_vars, error::ErrorKind, generalize::generalize_fixed,
  get_env::get_ty_info, st::St, ty, ty_var::fixed::TyVarSrc,
};

pub(crate) fn get(
  st: &mut St,
  idx: sml_hir::Idx,
  bs: &Bs,
  marker: SymsMarker,
  ars: &sml_hir::Arenas,
  inner_env: &mut Env,
  kind: &sml_hir::WhereKind,
) {
  match kind {
    sml_hir::WhereKind::Type(ty_vars, path, ty) => {
      let mut cx = bs.as_cx();
      let fixed = add_fixed_ty_vars(st, idx, &mut cx, TyVarSrc::Ty, ty_vars);
      let mut ty_scheme = TyScheme::zero(ty::get(st, &cx, ars, ty::Mode::TyRhs, *ty));
      generalize_fixed(fixed, &mut ty_scheme);
      get_where_type(st, idx, marker, inner_env, path, ty_scheme, true);
    }
    sml_hir::WhereKind::Structure(lhs, rhs) => {
      let lhs_ty_cons = ty_con_paths::get(inner_env, lhs);
      for e in lhs_ty_cons.disallow {
        st.err(idx, e.into());
      }
      let lhs_ty_cons = match lhs_ty_cons.val {
        Ok(x) => x,
        Err(e) => {
          st.err(idx, e.into());
          return;
        }
      };
      let rhs_ty_cons = ty_con_paths::get(&bs.env, rhs);
      for e in rhs_ty_cons.disallow {
        st.err(idx, e.into());
      }
      let rhs_ty_cons = match rhs_ty_cons.val {
        Ok(x) => x,
        Err(e) => {
          st.err(idx, e.into());
          return;
        }
      };
      for ty_con in lhs_ty_cons {
        if !rhs_ty_cons.contains(&ty_con) {
          continue;
        }
        let lhs = ty_con_paths::join_paths(lhs, &ty_con);
        let rhs = ty_con_paths::join_paths(rhs, &ty_con);
        let ty_info = get_ty_info(&bs.env, &rhs);
        for e in ty_info.disallow {
          st.err(idx, e.into());
        }
        match ty_info.val {
          Ok(ty_info) => {
            let ty_scheme = ty_info.ty_scheme.clone();
            get_where_type(st, idx, marker, inner_env, &lhs, ty_scheme, false);
          }
          Err(e) => st.err(idx, e.into()),
        }
      }
    }
  }
}

/// HACK: we allow intentionally ignoring cannot realize ty errors. I'm not exactly sure of the
/// semantics of `where S = T` but this silences some errors seen in valid NJ-flavored SML.
fn get_where_type(
  st: &mut St,
  idx: sml_hir::Idx,
  marker: SymsMarker,
  inner_env: &mut Env,
  path: &sml_path::Path,
  ty_scheme: TyScheme,
  emit_cannot_realize: bool,
) {
  let ty_info = get_ty_info(inner_env, path);
  for e in ty_info.disallow {
    st.err(idx, e.into());
  }
  let path_ty_scheme = match ty_info.val {
    Ok(x) => &x.ty_scheme,
    Err(e) => {
      st.err(idx, e.into());
      return;
    }
  };
  let want = path_ty_scheme.bound_vars.len();
  let got = ty_scheme.bound_vars.len();
  if want != got {
    st.err(idx, ErrorKind::WrongNumTyArgs(want, got));
    return;
  }
  match &path_ty_scheme.ty {
    Ty::None => {}
    Ty::Con(_, sym) => {
      // TODO well-formed check - need to check every ty info in the resulting env has either empty
      // val env or the ty scheme is a Con?
      if sym.generated_after(marker) {
        let mut subst = realize::TyRealization::default();
        subst.insert(*sym, ty_scheme);
        realize::get_env(&subst, inner_env);
      } else {
        // @test(sig::impossible)
        if emit_cannot_realize {
          st.err(idx, ErrorKind::CannotRealizeTy(path.clone(), path_ty_scheme.clone()));
        }
      }
    }
    // @test(sig::where_not_con)
    _ => {
      if emit_cannot_realize {
        st.err(idx, ErrorKind::CannotRealizeTy(path.clone(), path_ty_scheme.clone()));
      }
    }
  }
}
