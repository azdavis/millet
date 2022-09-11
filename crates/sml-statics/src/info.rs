use crate::types::{Def, DefPath, MetaVarInfo, MetaVarNames, Syms, Ty, TyScheme};
use crate::util::ty_syms;
use fast_hash::FxHashMap;
use std::fmt::Write as _;

/// Information about HIR indices.
#[derive(Debug, Clone)]
pub struct Info {
  mode: Mode,
  store: FxHashMap<sml_hir::Idx, InfoEntry>,
  pub(crate) meta_vars: MetaVarInfo,
}

#[derive(Debug, Clone)]
pub(crate) struct TyEntry {
  pub(crate) ty: Ty,
  pub(crate) ty_scheme: Option<TyScheme>,
}

#[derive(Debug, Default, Clone)]
struct InfoEntry {
  ty_entry: Option<TyEntry>,
  def: Option<Def>,
  doc: Option<String>,
}

impl Info {
  pub(crate) fn new(mode: Mode) -> Self {
    Self { mode, store: FxHashMap::default(), meta_vars: MetaVarInfo::default() }
  }

  pub(crate) fn insert(&mut self, idx: sml_hir::Idx, ty_entry: Option<TyEntry>, def: Option<Def>) {
    // ignore ty schemes that bind no vars
    let entry = InfoEntry {
      ty_entry: ty_entry.map(|mut ty_entry| {
        ty_entry.ty_scheme =
          ty_entry.ty_scheme.and_then(|x| (!x.bound_vars.is_empty()).then_some(x));
        ty_entry
      }),
      def,
      doc: None,
    };
    assert!(self.store.insert(idx, entry).is_none());
  }

  /// Add documentation to an index. Returns the old doc.
  pub fn add_doc(&mut self, idx: sml_hir::Idx, doc: String) -> Option<String> {
    self.store.entry(idx).or_default().doc.replace(doc)
  }

  pub(crate) fn tys_mut(&mut self) -> impl Iterator<Item = &mut Ty> {
    self.store.values_mut().filter_map(|entry| entry.ty_entry.as_mut().map(|x| &mut x.ty))
  }

  pub(crate) fn mode(&self) -> &Mode {
    &self.mode
  }

  /// Returns information about meta type variables.
  pub fn meta_vars(&self) -> &MetaVarInfo {
    &self.meta_vars
  }

  /// Returns a Markdown string with type information associated with this index.
  pub fn get_ty_md(&self, syms: &Syms, idx: sml_hir::Idx) -> Option<String> {
    let mut ret = String::new();
    self.get_ty_md_(&mut ret, syms, idx)?;
    Some(ret)
  }

  fn get_ty_md_(&self, s: &mut String, syms: &Syms, idx: sml_hir::Idx) -> Option<()> {
    let ty_entry = self.store.get(&idx)?.ty_entry.as_ref()?;
    let mut mvs = MetaVarNames::new(&self.meta_vars);
    mvs.extend_for(&ty_entry.ty);
    writeln!(s, "```sml").unwrap();
    if let Some(ty_scheme) = &ty_entry.ty_scheme {
      mvs.extend_for(&ty_scheme.ty);
      let ty_scheme = ty_scheme.display(&mvs, syms);
      writeln!(s, "(* most general *)").unwrap();
      writeln!(s, "{ty_scheme}").unwrap();
      writeln!(s, "(* this usage *)").unwrap();
    }
    let ty = ty_entry.ty.display(&mvs, syms);
    writeln!(s, "{ty}").unwrap();
    writeln!(s, "```").unwrap();
    Some(())
  }

  /// Returns documentation for this index.
  pub fn get_doc(&self, idx: sml_hir::Idx) -> Option<&str> {
    self.store.get(&idx)?.doc.as_deref()
  }

  /// Returns the definition site of the idx.
  pub fn get_def(&self, idx: sml_hir::Idx) -> Option<Def> {
    self.store.get(&idx)?.def
  }

  /// Returns the definition site of the type for the idx.
  pub fn get_ty_defs(&self, syms: &Syms, idx: sml_hir::Idx) -> Option<Vec<Def>> {
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

  /// Gets the variants for the type of the index. The bool is whether the name has an argument.
  pub fn get_variants(&self, syms: &Syms, idx: sml_hir::Idx) -> Option<Vec<(sml_hir::Name, bool)>> {
    let ty_entry = self.store.get(&idx)?.ty_entry.as_ref()?;
    let sym = match ty_entry.ty {
      Ty::Con(_, x) => x,
      _ => return None,
    };
    let (_, ty_info) = syms.get(&sym)?;
    let mut ret: Vec<_> = ty_info
      .val_env
      .iter()
      .map(|(name, val_info)| {
        let has_arg = matches!(val_info.ty_scheme.ty, Ty::Fn(_, _));
        (name.clone(), has_arg)
      })
      .collect();
    ret.sort_unstable();
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
  /// The string is the name of the std basis file.
  StdBasis(&'static str),
}

impl Mode {
  pub(crate) fn is_regular(&self) -> bool {
    matches!(self, Self::Regular(_))
  }

  pub(crate) fn is_std_basis(&self) -> bool {
    matches!(self, Self::StdBasis(_))
  }

  pub(crate) fn path(&self) -> Option<DefPath> {
    match self {
      Self::Regular(p) => p.map(DefPath::Regular),
      Self::StdBasis(name) => Some(DefPath::StdBasis(name)),
    }
  }
}
