//! See [`Info`].

use crate::basis::Bs;
use sml_hir::la_arena;
use sml_statics_types::ty::{Ty, TyData, TyScheme};
use sml_statics_types::util::ty_syms;
use sml_statics_types::{def, env::Env, mode::Mode};
use std::fmt;

pub(crate) type IdxMap<K, V> = la_arena::ArenaMap<la_arena::Idx<K>, V>;

pub(crate) type DefMap<K> = IdxMap<K, def::Set>;

#[derive(Debug, Default, Clone)]
pub(crate) struct Defs {
  pub(crate) str_exp: DefMap<sml_hir::StrExp>,
  pub(crate) sig_exp: DefMap<sml_hir::SigExp>,
  pub(crate) dec: DefMap<sml_hir::Dec>,
  pub(crate) exp: DefMap<sml_hir::Exp>,
  pub(crate) pat: DefMap<sml_hir::Pat>,
  pub(crate) ty: DefMap<sml_hir::Ty>,
}

impl Defs {
  fn get(&self, idx: sml_hir::Idx) -> Option<&def::Set> {
    match idx {
      sml_hir::Idx::StrDec(_) | sml_hir::Idx::Spec(_) => None,
      sml_hir::Idx::StrExp(idx) => self.str_exp.get(idx),
      sml_hir::Idx::SigExp(idx) => self.sig_exp.get(idx),
      sml_hir::Idx::Dec(idx) => self.dec.get(idx),
      sml_hir::Idx::Exp(idx) => self.exp.get(idx),
      sml_hir::Idx::Pat(idx) => self.pat.get(idx),
      sml_hir::Idx::Ty(idx) => self.ty.get(idx),
    }
  }

  fn with_def(&self, def: def::Def) -> impl Iterator<Item = sml_hir::Idx> {
    std::iter::empty::<(sml_hir::Idx, &def::Set)>()
      .chain(self.str_exp.iter().map(|(idx, set)| (idx.into(), set)))
      .chain(self.sig_exp.iter().map(|(idx, set)| (idx.into(), set)))
      .chain(self.dec.iter().map(|(idx, set)| (idx.into(), set)))
      .chain(self.exp.iter().map(|(idx, set)| (idx.into(), set)))
      .chain(self.pat.iter().map(|(idx, set)| (idx.into(), set)))
      .chain(self.ty.iter().map(|(idx, set)| (idx.into(), set)))
      .flat_map(|(idx, defs)| defs.iter().map(move |&def| (idx, def)))
      .filter_map(move |(idx, d)| (d == def).then_some(idx))
  }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct Docs {
  pub(crate) str_dec: IdxMap<sml_hir::StrDec, String>,
  pub(crate) spec: IdxMap<sml_hir::Spec, String>,
  pub(crate) dec: IdxMap<sml_hir::Dec, String>,
  pub(crate) pat: IdxMap<sml_hir::Pat, String>,
}

impl Docs {
  fn get(&self, idx: sml_hir::Idx) -> Option<&str> {
    let ret = match idx {
      sml_hir::Idx::StrDec(idx) => self.str_dec.get(idx)?,
      sml_hir::Idx::Spec(idx) => self.spec.get(idx)?,
      sml_hir::Idx::Dec(idx) => self.dec.get(idx)?,
      sml_hir::Idx::Pat(idx) => self.pat.get(idx)?,
      _ => return None,
    };
    Some(ret.as_str())
  }

  fn try_insert(&mut self, idx: sml_hir::Idx, doc: String) {
    match idx {
      sml_hir::Idx::StrDec(idx) => {
        self.str_dec.insert(idx, doc);
      }
      sml_hir::Idx::Spec(idx) => {
        self.spec.insert(idx, doc);
      }
      sml_hir::Idx::Dec(idx) => {
        self.dec.insert(idx, doc);
      }
      sml_hir::Idx::Pat(idx) => {
        self.pat.insert(idx, doc);
      }
      _ => {}
    }
  }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct TyEntries {
  pub(crate) exp: IdxMap<sml_hir::Exp, TyEntry>,
  pub(crate) pat: IdxMap<sml_hir::Pat, TyEntry>,
  pub(crate) ty: IdxMap<sml_hir::Ty, TyEntry>,
}

impl TyEntries {
  fn get(&self, idx: sml_hir::Idx) -> Option<&TyEntry> {
    match idx {
      sml_hir::Idx::Exp(idx) => self.exp.get(idx),
      sml_hir::Idx::Pat(idx) => self.pat.get(idx),
      sml_hir::Idx::Ty(idx) => self.ty.get(idx),
      _ => None,
    }
  }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct Entries {
  pub(crate) defs: Defs,
  pub(crate) docs: Docs,
  pub(crate) tys: TyEntries,
}

/// Information about HIR indices.
#[derive(Debug, Clone)]
pub struct Info {
  pub(crate) mode: Mode,
  pub(crate) entries: Entries,
  pub(crate) bs: Bs,
}

impl Info {
  pub(crate) fn new(mode: Mode) -> Self {
    Self { mode, entries: Entries::default(), bs: Bs::default() }
  }

  /// Returns the basis.
  #[must_use]
  pub fn basis(&self) -> &Bs {
    &self.bs
  }

  /// Adds documentation for the index.
  pub fn add_doc(&mut self, idx: sml_hir::Idx, doc: String) {
    self.entries.docs.try_insert(idx, doc);
  }

  /// Returns a Markdown string with type information associated with this index.
  #[must_use]
  pub fn get_ty_md(
    &self,
    st: &sml_statics_types::St,
    idx: sml_hir::Idx,
    lines: config::DiagnosticLines,
  ) -> Option<String> {
    let ty_entry = self.entries.tys.get(idx)?;
    let ty_entry = TyEntryDisplay { ty_entry, st, lines };
    Some(ty_entry.to_string())
  }

  /// Returns documentation for this index.
  #[must_use]
  pub fn get_doc(&self, idx: sml_hir::Idx) -> Option<&str> {
    self.entries.docs.get(idx)
  }

  /// Returns the definition sites of the idx.
  #[must_use]
  pub fn get_defs(&self, idx: sml_hir::Idx) -> Option<&def::Set> {
    self.entries.defs.get(idx)
  }

  /// Returns the definition sites of the type for the idx.
  #[must_use]
  pub fn get_ty_defs(&self, st: &sml_statics_types::St, idx: sml_hir::Idx) -> Option<def::Set> {
    let ty_entry = self.entries.tys.get(idx)?;
    let mut ret = def::Set::new();
    ty_syms(&st.tys, ty_entry.ty, &mut |sym| {
      let sym_info = st.syms.get(sym);
      let defs = sym_info.iter().flat_map(|sym_info| sym_info.ty_info.defs.iter()).copied();
      ret.extend(defs);
    });
    Some(ret)
  }

  /// Gets the variants for the type of the index. The bool is whether the name has an argument.
  #[must_use]
  pub fn get_variants(
    &self,
    st: &sml_statics_types::St,
    idx: sml_hir::Idx,
  ) -> Option<Vec<(str_util::Name, bool)>> {
    let ty_entry = self.entries.tys.get(idx)?;
    let sym = match st.tys.data(ty_entry.ty) {
      TyData::Con(data) => data.sym,
      _ => return None,
    };
    let ret: Vec<_> = st
      .syms
      .get(sym)?
      .ty_info
      .val_env
      .iter()
      .map(|(name, val_info)| {
        let has_arg = matches!(st.tys.data(val_info.ty_scheme.ty), TyData::Fn(_));
        (name.clone(), has_arg)
      })
      .collect();
    Some(ret)
  }

  /// Returns the symbols for this file.
  ///
  /// You also have to pass down the `path` that this `Info` is for. It's slightly odd, but we
  /// need it to know which `Def`s we should actually include in the return value.
  #[must_use]
  pub fn document_symbols(
    &self,
    st: &sml_statics_types::St,
    path: paths::PathId,
  ) -> Vec<DocumentSymbol> {
    let mut ret = Vec::<DocumentSymbol>::new();
    ret.extend(self.bs.fun_env.iter().flat_map(|(name, fun_sig)| {
      fun_sig.body_env.defs.iter().filter_map(|&def| {
        let idx = def_idx(path, def)?;
        let mut children = Vec::<DocumentSymbol>::new();
        env_syms(&mut children, st, path, &fun_sig.body_env);
        Some(DocumentSymbol {
          name: name.as_str().to_owned(),
          kind: sml_namespace::SymbolKind::Functor,
          detail: None,
          idx,
          children,
        })
      })
    }));
    ret.extend(self.bs.sig_env.iter().flat_map(|(name, sig)| {
      sig.env.defs.iter().filter_map(|&def| {
        let idx = def_idx(path, def)?;
        let mut children = Vec::<DocumentSymbol>::new();
        env_syms(&mut children, st, path, &sig.env);
        Some(DocumentSymbol {
          name: name.as_str().to_owned(),
          kind: sml_namespace::SymbolKind::Signature,
          detail: None,
          idx,
          children,
        })
      })
    }));
    env_syms(&mut ret, st, path, &self.bs.env);
    // order doesn't seem to matter. at least vs code displays the symbols in source order.
    ret
  }

  /// Returns indices that have the given definition.
  pub fn get_with_def(&self, def: def::Def) -> impl Iterator<Item = sml_hir::Idx> {
    self.entries.defs.with_def(def)
  }

  /// Returns a string representation of a type annotation for the pattern.
  #[must_use]
  pub fn show_pat_ty_annot(
    &self,
    st: &sml_statics_types::St,
    pat: sml_hir::la_arena::Idx<sml_hir::Pat>,
  ) -> Option<String> {
    let ty_entry = self.entries.tys.pat.get(pat)?;
    let ty = ty_entry.ty.display(st, config::DiagnosticLines::One);
    Some(format!(" : {ty}"))
  }

  /// Returns a string representation of a type annotation for the expression.
  #[must_use]
  pub fn show_ty_annot(
    &self,
    st: &sml_statics_types::St,
    exp: sml_hir::la_arena::Idx<sml_hir::Exp>,
  ) -> Option<String> {
    let ty_entry = self.entries.tys.exp.get(exp)?;
    let ty = ty_entry.ty.display(st, config::DiagnosticLines::One);
    Some(format!(" : {ty}"))
  }
}

#[derive(Debug, Clone)]
pub(crate) struct TyEntry {
  ty: Ty,
  /// invariant: if this is `Some`, the ty scheme has non-empty bound ty vars.
  ty_scheme: Option<TyScheme>,
}

impl TyEntry {
  pub(crate) fn new(ty: Ty, ty_scheme: Option<TyScheme>) -> Self {
    Self { ty, ty_scheme: ty_scheme.and_then(|ts| (!ts.bound_vars.is_empty()).then_some(ts)) }
  }
}

struct TyEntryDisplay<'a> {
  ty_entry: &'a TyEntry,
  st: &'a sml_statics_types::St,
  lines: config::DiagnosticLines,
}

impl fmt::Display for TyEntryDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "```sml")?;
    if let Some(ty_scheme) = &self.ty_entry.ty_scheme {
      let ty_scheme = ty_scheme.display(self.st, self.lines);
      writeln!(f, "(* most general *)")?;
      writeln!(f, "{ty_scheme}")?;
      writeln!(f, "(* this usage *)")?;
    }
    let ty = self.ty_entry.ty.display(self.st, self.lines);
    writeln!(f, "{ty}")?;
    writeln!(f, "```")?;
    Ok(())
  }
}

/// need to do extend instead of a big chain of chains because of the borrow checker.
fn env_syms(
  ac: &mut Vec<DocumentSymbol>,
  st: &sml_statics_types::St,
  path: paths::PathId,
  env: &Env,
) {
  ac.extend(env.str_env.iter().flat_map(|(name, env)| {
    env.defs.iter().filter_map(|&def| {
      let idx = def_idx(path, def)?;
      let mut children = Vec::<DocumentSymbol>::new();
      env_syms(&mut children, st, path, env);
      Some(DocumentSymbol {
        name: name.as_str().to_owned(),
        kind: sml_namespace::SymbolKind::Structure,
        detail: None,
        idx,
        children,
      })
    })
  }));
  ac.extend(env.ty_env.iter().flat_map(|(name, ty_info)| {
    ty_info.defs.iter().filter_map(|&def| {
      let idx = def_idx(path, def)?;
      let ty_scheme = ty_info.ty_scheme.display(st, config::DiagnosticLines::Many);
      Some(DocumentSymbol {
        name: name.as_str().to_owned(),
        kind: sml_namespace::SymbolKind::Type,
        detail: Some(ty_scheme.to_string()),
        idx,
        children: Vec::new(),
      })
    })
  }));
  ac.extend(env.val_env.iter().flat_map(|(name, val_info)| {
    let detail = val_info.ty_scheme.display(st, config::DiagnosticLines::Many).to_string();
    val_info.defs.iter().filter_map(move |&def| {
      let idx = def_idx(path, def)?;
      Some(DocumentSymbol {
        name: name.as_str().to_owned(),
        kind: sml_symbol_kind::get(&st.tys, val_info),
        detail: Some(detail.clone()),
        idx,
        children: Vec::new(),
      })
    })
  }));
}

fn def_idx(path: paths::PathId, def: def::Def) -> Option<sml_hir::Idx> {
  match def {
    def::Def::Path(p, idx) => match p {
      def::Path::Regular(p) => (p == path).then_some(idx),
      def::Path::BuiltinLib(_) => None,
    },
    def::Def::Primitive(_) => None,
  }
}

/// A document symbol.
#[derive(Debug)]
pub struct DocumentSymbol {
  /// The name of the symbol.
  pub name: String,
  /// What kind of symbol this is.
  pub kind: sml_namespace::SymbolKind,
  /// Detail about this symbol.
  pub detail: Option<String>,
  /// The index of the symbol.
  pub idx: sml_hir::Idx,
  /// Children of this symbol.
  pub children: Vec<DocumentSymbol>,
}
