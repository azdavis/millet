use crate::types::{Def, DefPath, Syms, Ty, TyScheme};
use crate::util::ty_syms;
use fast_hash::FxHashMap;
use std::fmt::Write as _;

/// Information about HIR indices.
#[derive(Debug, Clone)]
pub struct Info {
  mode: Mode,
  store: FxHashMap<hir::Idx, InfoEntry>,
}

#[derive(Debug, Clone)]
pub(crate) struct TyEntry {
  pub(crate) ty: Ty,
  pub(crate) ty_scheme: Option<TyScheme>,
}

#[derive(Debug, Clone)]
struct InfoEntry {
  ty_entry: Option<TyEntry>,
  def: Option<Def>,
}

impl Info {
  pub(crate) fn new(mode: Mode) -> Self {
    Self {
      mode,
      store: FxHashMap::default(),
    }
  }

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

  pub(crate) fn mode(&self) -> &Mode {
    &self.mode
  }

  /// Returns a Markdown string with type information associated with this index.
  pub fn get_ty_md(&self, syms: &Syms, idx: hir::Idx) -> Option<String> {
    let mut ret = String::new();
    self.get_ty_md_(&mut ret, syms, idx)?;
    Some(ret)
  }

  fn get_ty_md_(&self, s: &mut String, syms: &Syms, idx: hir::Idx) -> Option<()> {
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

  /// Returns documentation for this index.
  pub fn get_doc(&self, idx: hir::Idx) -> Option<&str> {
    match &self.mode {
      Mode::Regular(_) => None,
      Mode::StdBasis(_, comments) => comments.get(&idx).map(String::as_str),
    }
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

/// The mode for checking.
#[derive(Debug, Clone)]
pub enum Mode {
  /// Regular checking. The default.
  Regular(Option<paths::PathId>),
  /// Standard basis checking. Notably, ascription structure expressions will not check to see if
  /// they actually match the signature.
  ///
  /// Pass a map from hir indices to comments to have this be included in the [`Info`] returned.
  StdBasis(&'static str, FxHashMap<hir::Idx, String>),
}

impl Mode {
  pub(crate) fn is_regular(&self) -> bool {
    matches!(self, Self::Regular(_))
  }

  pub(crate) fn path(&self) -> Option<DefPath> {
    match self {
      Self::Regular(p) => p.map(DefPath::Regular),
      Self::StdBasis(name, _) => Some(DefPath::StdBasis(name)),
    }
  }
}
