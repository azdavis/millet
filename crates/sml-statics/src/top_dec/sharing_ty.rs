//! Dealing with `sharing` and `sharing type`.

use crate::types::{Sym, SymsMarker, Ty, TyScheme};
use crate::{env::Env, equality, error::ErrorKind, get_env::get_ty_info, st::St, top_dec::realize};

/// `sharing type` directly uses this, and the `sharing` derived form eventually uses this.
pub(crate) fn get(
  st: &mut St,
  idx: sml_hir::Idx,
  marker: SymsMarker,
  inner_env: &mut Env,
  paths: &[sml_hir::Path],
) {
  let mut ac = None::<SharingTyScheme>;
  let mut syms = Vec::<Sym>::with_capacity(paths.len());
  for path in paths {
    let ty_scheme = match get_ty_info(inner_env, path) {
      Ok(x) => &x.ty_scheme,
      Err(e) => {
        st.err(idx, e);
        continue;
      }
    };
    let sym = match &ty_scheme.ty {
      Ty::Con(_, x) => *x,
      _ => {
        st.err(idx, ErrorKind::CannotShareTy(path.clone(), ty_scheme.clone()));
        continue;
      }
    };
    if !sym.generated_after(marker) {
      st.err(idx, ErrorKind::CannotShareTy(path.clone(), ty_scheme.clone()));
      continue;
    }
    match &ac {
      None => ac = Some(SharingTyScheme::new(st, ty_scheme.clone())),
      Some(cur_ac) => {
        if !cur_ac.equality {
          let new = SharingTyScheme::new(st, ty_scheme.clone());
          if new.equality {
            ac = Some(new);
          }
        }
      }
    }
    syms.push(sym);
  }
  match ac {
    Some(ac) => {
      let subst: realize::TyRealization =
        syms.into_iter().map(|sym| (sym, ac.ty_scheme.clone())).collect();
      realize::get_env(st, idx, &subst, inner_env);
    }
    None => log::info!("should have already errored"),
  }
}

struct SharingTyScheme {
  ty_scheme: TyScheme,
  equality: bool,
}

impl SharingTyScheme {
  fn new(st: &mut St, ty_scheme: TyScheme) -> Self {
    let equality = equality::get_ty_scheme(st, ty_scheme.clone()).is_ok();
    Self { ty_scheme, equality }
  }
}
