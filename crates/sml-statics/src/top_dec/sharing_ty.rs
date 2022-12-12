//! Dealing with `sharing` and `sharing type`.

use crate::types::{SymsMarker, Ty, TyScheme};
use crate::{env::Env, equality, error::ErrorKind, get_env::get_ty_info, st::St, top_dec::realize};
use fast_hash::FxHashSet;

/// `sharing type` directly uses this, and the `sharing` derived form eventually uses this.
pub(crate) fn get(
  st: &mut St,
  idx: sml_hir::Idx,
  marker: SymsMarker,
  inner_env: &mut Env,
  paths: &[sml_hir::Path],
) {
  let mut ac = None::<SharingTyScheme>;
  let syms = paths.iter().filter_map(|path| {
    let ty_scheme = match get_ty_info(inner_env, path) {
      Ok(x) => &x.ty_scheme,
      Err(e) => {
        st.err(idx, e);
        return None;
      }
    };
    let sym = match &ty_scheme.ty {
      Ty::Con(_, x) => *x,
      _ => {
        st.err(idx, ErrorKind::CannotShareTy(path.clone(), ty_scheme.clone()));
        return None;
      }
    };
    if !sym.generated_after(marker) {
      st.err(idx, ErrorKind::CannotShareTy(path.clone(), ty_scheme.clone()));
      return None;
    }
    match &ac {
      None => ac = Some(SharingTyScheme::new(st, ty_scheme.clone())),
      Some(cur_ac) => {
        let want = cur_ac.ty_scheme.bound_vars.len();
        let got = ty_scheme.bound_vars.len();
        if want != got {
          st.err(idx, ErrorKind::WrongNumTyArgs(want, got));
          return None;
        }
        if !cur_ac.equality {
          let new = SharingTyScheme::new(st, ty_scheme.clone());
          if new.equality {
            ac = Some(new);
          }
        }
      }
    }
    Some(sym)
  });
  let syms: FxHashSet<_> = syms.collect();
  // the None case is possible, but we should have errored already.
  //
  // @test(deviations::smlnj::sharing_via_abbreviation_short)
  if let Some(ac) = ac {
    let mut subst = realize::TyRealization::default();
    for sym in syms {
      subst.insert(sym, ac.ty_scheme.clone());
    }
    realize::get_env(&subst, inner_env);
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
