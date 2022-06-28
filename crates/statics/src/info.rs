use crate::types::{Def, Syms, Ty, TyScheme};
use crate::util::ty_syms;
use fast_hash::FxHashMap;
use std::fmt::Write as _;

/// Information about HIR indices.
#[derive(Debug, Default)]
pub struct Info {
  store: FxHashMap<hir::Idx, InfoEntry>,
}

#[derive(Debug)]
pub(crate) struct TyEntry {
  pub(crate) ty: Ty,
  pub(crate) ty_scheme: Option<TyScheme>,
}

#[derive(Debug)]
struct InfoEntry {
  ty_entry: Option<TyEntry>,
  def: Option<Def>,
}

impl Info {
  pub(crate) fn insert<I>(&mut self, idx: I, ty_entry: Option<TyEntry>, def: Option<Def>)
  where
    I: Into<hir::Idx>,
  {
    let idx = idx.into();
    // ignore ty schemes that bind no vars
    let entry = InfoEntry {
      ty_entry: ty_entry.map(|mut ty_entry| {
        ty_entry.ty_scheme = ty_entry
          .ty_scheme
          .and_then(|x| (!x.bound_vars.is_empty()).then(|| x));
        ty_entry
      }),
      def,
    };
    assert!(self.store.insert(idx, entry).is_none());
  }

  pub(crate) fn tys_mut(&mut self) -> impl Iterator<Item = &mut Ty> {
    self
      .store
      .values_mut()
      .filter_map(|entry| entry.ty_entry.as_mut().map(|x| &mut x.ty))
  }

  /// Returns a Markdown string with information associated with this index.
  pub fn get_md(&self, syms: &Syms, idx: hir::Idx) -> Option<String> {
    let mut ret = String::new();
    self.get_ty_md(&mut ret, syms, idx)?;
    Some(ret)
  }

  fn get_ty_md(&self, s: &mut String, syms: &Syms, idx: hir::Idx) -> Option<()> {
    let ty_entry = self.store.get(&idx)?.ty_entry.as_ref()?;
    let mvs = ty_entry.ty.meta_var_names();
    let ty = ty_entry.ty.display(&mvs, syms);
    writeln!(s, "```sml").unwrap();
    if let Some(ty_scheme) = &ty_entry.ty_scheme {
      let mvs = ty_scheme.ty.meta_var_names();
      let ty_scheme = ty_scheme.display(&mvs, syms);
      writeln!(s, "(* most general *)").unwrap();
      writeln!(s, "{ty_scheme}").unwrap();
      writeln!(s, "(* this usage *)").unwrap();
    }
    writeln!(s, "{ty}").unwrap();
    writeln!(s, "```").unwrap();
    Some(())
  }

  /// Returns the definition site of the idx.
  pub fn get_def(&self, idx: hir::Idx) -> Option<Def> {
    self.store.get(&idx)?.def
  }

  /// Returns the definition site of the type for the idx.
  pub fn get_ty_defs(&self, syms: &Syms, idx: hir::Idx) -> Option<Vec<Def>> {
    let ty_entry = self.store.get(&idx)?.ty_entry.as_ref()?;
    let mut ret = Vec::<Def>::new();
    ty_syms(
      &mut |sym| match syms.get(&sym) {
        None => {}
        Some((_, ty_info)) => match ty_info.def {
          None => {}
          Some(def) => ret.push(def),
        },
      },
      &ty_entry.ty,
    );
    Some(ret)
  }
}
