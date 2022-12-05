//! Dealing with `where` and `where type`.

use crate::env::{Bs, Env};
use crate::top_dec::{realize, ty_con_paths};
use crate::types::{SymsMarker, Ty, TyScheme, TyVarSrc};
use crate::{
  dec::add_fixed_ty_vars, error::ErrorKind, generalize::generalize_fixed, get_env::get_ty_info,
  st::St, ty,
};
use fast_hash::map;

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
      match get_where_type(st, idx, marker, inner_env, path, ty_scheme) {
        Ok(()) => {}
        Err(e) => st.err(idx, e),
      }
    }
    sml_hir::WhereKind::Structure(lhs, rhs) => {
      let lhs_ty_cons = match ty_con_paths::get(inner_env, lhs) {
        Ok(x) => x,
        Err(e) => {
          st.err(idx, e);
          return;
        }
      };
      let rhs_ty_cons = match ty_con_paths::get(&bs.env, rhs) {
        Ok(x) => x,
        Err(e) => {
          st.err(idx, e);
          return;
        }
      };
      for ty_con in lhs_ty_cons {
        if !rhs_ty_cons.contains(&ty_con) {
          continue;
        }
        let lhs = ty_con_paths::join_paths(lhs, &ty_con);
        let rhs = ty_con_paths::join_paths(rhs, &ty_con);
        match get_ty_info(&bs.env, &rhs) {
          Ok(ty_info) => {
            let ty_scheme = ty_info.ty_scheme.clone();
            // HACK: intentionally ignore CannotRealizeTy. I'm not exactly sure of the semantics of
            // `where S = T` but this silences some errors seen in valid NJ-flavored SML.
            match get_where_type(st, idx, marker, inner_env, &lhs, ty_scheme) {
              Ok(()) | Err(ErrorKind::CannotRealizeTy(_, _)) => {}
              Err(e) => st.err(idx, e),
            }
          }
          Err(e) => st.err(idx, e),
        }
      }
    }
  }
}

fn get_where_type(
  st: &mut St,
  idx: sml_hir::Idx,
  marker: SymsMarker,
  inner_env: &mut Env,
  path: &sml_hir::Path,
  ty_scheme: TyScheme,
) -> Result<(), ErrorKind> {
  let ty_info = get_ty_info(inner_env, path)?;
  match &ty_info.ty_scheme.ty {
    Ty::None => Ok(()),
    // TODO side condition for well-formed?
    Ty::Con(_, sym) => {
      if sym.generated_after(marker) {
        realize::get_env(st, idx, &map([(*sym, ty_scheme)]), inner_env);
        Ok(())
      } else {
        // @test(sig::impossible)
        Err(ErrorKind::CannotRealizeTy(path.clone(), ty_info.ty_scheme.clone()))
      }
    }
    // @test(sig::where_not_con)
    _ => Err(ErrorKind::CannotRealizeTy(path.clone(), ty_info.ty_scheme.clone())),
  }
}
