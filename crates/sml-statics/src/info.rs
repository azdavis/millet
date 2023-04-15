//! See [`Info`].

use crate::{basis::Bs, env::Env};
use fast_hash::{FxHashMap, FxHashSet};
use sml_statics_types::info::{IdStatus, ValInfo};
use sml_statics_types::ty::{Ty, TyData, TyScheme, Tys};
use sml_statics_types::util::ty_syms;
use sml_statics_types::{def, display::MetaVarNames, mode::Mode, sym::Syms};
use std::fmt;

/// Information about HIR indices.
#[derive(Debug, Clone)]
pub struct Info {
  pub(crate) mode: Mode,
  pub(crate) indices: FxHashMap<sml_hir::Idx, IdxEntry>,
  pub(crate) bs: Bs,
}

impl Info {
  pub(crate) fn new(mode: Mode) -> Self {
    Self { mode, indices: FxHashMap::default(), bs: Bs::default() }
  }

  pub(crate) fn insert(
    &mut self,
    idx: sml_hir::Idx,
    ty_entry: Option<TyEntry>,
    defs: FxHashSet<def::Def>,
  ) {
    let entry = IdxEntry { ty_entry, defs, doc: None };
    assert!(self.indices.insert(idx, entry).is_none());
  }

  /// Add documentation to an index. Returns the old doc.
  pub fn add_doc(&mut self, idx: sml_hir::Idx, doc: String) -> Option<String> {
    self.indices.entry(idx).or_default().doc.replace(doc)
  }

  /// Returns a Markdown string with type information associated with this index.
  #[must_use]
  pub fn get_ty_md(&self, syms: &Syms, tys: &Tys, idx: sml_hir::Idx) -> Option<String> {
    let ty_entry = self.indices.get(&idx)?.ty_entry.as_ref()?;
    let ty_entry = TyEntryDisplay { ty_entry, syms, tys };
    Some(ty_entry.to_string())
  }

  /// Returns documentation for this index.
  #[must_use]
  pub fn get_doc(&self, idx: sml_hir::Idx) -> Option<&str> {
    self.indices.get(&idx)?.doc.as_deref()
  }

  /// Returns the definition sites of the idx.
  pub fn get_defs(&self, idx: sml_hir::Idx) -> impl Iterator<Item = def::Def> + '_ {
    self.indices.get(&idx).into_iter().flat_map(|x| &x.defs).copied()
  }

  /// Returns the definition site of the type for the idx.
  #[must_use]
  pub fn get_ty_defs(&self, syms: &Syms, tys: &Tys, idx: sml_hir::Idx) -> Option<Vec<def::Def>> {
    let ty_entry = self.indices.get(&idx)?.ty_entry.as_ref()?;
    let mut ret = Vec::<def::Def>::new();
    ty_syms(tys, ty_entry.ty, &mut |sym| match syms.get(sym) {
      None => {}
      Some(sym_info) => match sym_info.ty_info.def {
        None => {}
        Some(def) => ret.push(def),
      },
    });
    Some(ret)
  }

  /// Gets the variants for the type of the index. The bool is whether the name has an argument.
  #[must_use]
  pub fn get_variants(
    &self,
    syms: &Syms,
    tys: &Tys,
    idx: sml_hir::Idx,
  ) -> Option<Vec<(str_util::Name, bool)>> {
    let ty_entry = self.indices.get(&idx)?.ty_entry.as_ref()?;
    let sym = match tys.data(ty_entry.ty) {
      TyData::Con(data) => data.sym,
      _ => return None,
    };
    let mut ret: Vec<_> = syms
      .get(sym)?
      .ty_info
      .val_env
      .iter()
      .map(|(name, val_info)| {
        let has_arg = matches!(tys.data(val_info.ty_scheme.ty), TyData::Fn(_));
        (name.clone(), has_arg)
      })
      .collect();
    ret.sort_unstable();
    Some(ret)
  }

  /// Returns the symbols for this file.
  ///
  /// You also have to pass down the `path` that this `Info` is for. It's slightly odd, but we
  /// need it to know which `Def`s we should actually include in the return value.
  #[must_use]
  pub fn document_symbols(
    &self,
    syms: &Syms,
    tys: &Tys,
    path: paths::PathId,
  ) -> Vec<DocumentSymbol> {
    let mut mvs = MetaVarNames::new(tys);
    let mut ret = Vec::<DocumentSymbol>::new();
    ret.extend(self.bs.fun_env.iter().filter_map(|(name, fun_sig)| {
      let idx = def_idx(path, fun_sig.body_env.def?)?;
      let mut children = Vec::<DocumentSymbol>::new();
      env_syms(&mut children, &mut mvs, syms, tys, path, &fun_sig.body_env);
      Some(DocumentSymbol {
        name: name.as_str().to_owned(),
        kind: sml_namespace::SymbolKind::Functor,
        detail: None,
        idx,
        children,
      })
    }));
    ret.extend(self.bs.sig_env.iter().filter_map(|(name, sig)| {
      let idx = def_idx(path, sig.env.def?)?;
      let mut children = Vec::<DocumentSymbol>::new();
      env_syms(&mut children, &mut mvs, syms, tys, path, &sig.env);
      Some(DocumentSymbol {
        name: name.as_str().to_owned(),
        kind: sml_namespace::SymbolKind::Signature,
        detail: None,
        idx,
        children,
      })
    }));
    env_syms(&mut ret, &mut mvs, syms, tys, path, &self.bs.env);
    // order doesn't seem to matter. at least vs code displays the symbols in source order.
    ret
  }

  /// Returns indices that have the given definition.
  pub fn get_with_def(&self, def: def::Def) -> impl Iterator<Item = sml_hir::Idx> + '_ {
    self.indices.iter().filter_map(move |(&idx, entry)| entry.defs.contains(&def).then_some(idx))
  }

  /// Returns the completions for this file.
  #[must_use]
  pub fn completions(&self, syms: &Syms, tys: &Tys) -> Vec<CompletionItem> {
    let mut ret = Vec::<CompletionItem>::new();
    let mut mvs = MetaVarNames::new(tys);
    ret.extend(self.bs.env.val_env.iter().map(|(name, val_info)| {
      mvs.clear();
      mvs.extend_for(val_info.ty_scheme.ty);
      CompletionItem {
        label: name.as_str().to_owned(),
        kind: val_info_symbol_kind(tys, val_info),
        detail: Some(val_info.ty_scheme.display(&mvs, syms).to_string()),
        // TODO improve? might need to reorganize where documentation is stored
        documentation: None,
      }
    }));
    ret
  }

  /// Returns some type annotation bits.
  pub fn show_ty_annot<'a>(
    &'a self,
    syms: &'a Syms,
    tys: &'a Tys,
  ) -> impl Iterator<Item = (sml_hir::la_arena::Idx<sml_hir::Pat>, String)> + 'a {
    self.indices.iter().filter_map(|(&idx, entry)| match idx {
      sml_hir::Idx::Pat(pat) => {
        let self_def = entry.defs.iter().any(|&d| match d {
          def::Def::Path(_, ref_idx) => idx == ref_idx,
          def::Def::Primitive(_) => false,
        });
        if !self_def {
          return None;
        }
        let mut mvs = MetaVarNames::new(tys);
        let ty_entry = entry.ty_entry.as_ref()?;
        mvs.extend_for(ty_entry.ty);
        let ty = ty_entry.ty.display(&mvs, syms);
        Some((pat, format!(" : {ty})")))
      }
      _ => None,
    })
  }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct IdxEntry {
  ty_entry: Option<TyEntry>,
  defs: FxHashSet<def::Def>,
  doc: Option<String>,
}

#[derive(Debug, Clone)]
pub(crate) struct TyEntry {
  ty: Ty,
  /// invariant: if this is Some(_), the ty scheme has non-empty bound ty vars.
  ty_scheme: Option<TyScheme>,
}

impl TyEntry {
  pub(crate) fn new(ty: Ty, ty_scheme: Option<TyScheme>) -> Self {
    Self { ty, ty_scheme: ty_scheme.and_then(|ts| (!ts.bound_vars.is_empty()).then_some(ts)) }
  }
}

struct TyEntryDisplay<'a> {
  ty_entry: &'a TyEntry,
  syms: &'a Syms,
  tys: &'a Tys,
}

impl fmt::Display for TyEntryDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut mvs = MetaVarNames::new(self.tys);
    mvs.extend_for(self.ty_entry.ty);
    writeln!(f, "```sml")?;
    if let Some(ty_scheme) = &self.ty_entry.ty_scheme {
      mvs.extend_for(ty_scheme.ty);
      let ty_scheme = ty_scheme.display(&mvs, self.syms);
      writeln!(f, "(* most general *)")?;
      writeln!(f, "{ty_scheme}")?;
      writeln!(f, "(* this usage *)")?;
    }
    let ty = self.ty_entry.ty.display(&mvs, self.syms);
    writeln!(f, "{ty}")?;
    writeln!(f, "```")?;
    Ok(())
  }
}

/// need to do extend instead of a big chain of chains because of the borrow checker.
fn env_syms(
  ac: &mut Vec<DocumentSymbol>,
  mvs: &mut MetaVarNames<'_>,
  syms: &Syms,
  tys: &Tys,
  path: paths::PathId,
  env: &Env,
) {
  ac.extend(env.str_env.iter().filter_map(|(name, env)| {
    let idx = def_idx(path, env.def?)?;
    let mut children = Vec::<DocumentSymbol>::new();
    env_syms(&mut children, mvs, syms, tys, path, env);
    Some(DocumentSymbol {
      name: name.as_str().to_owned(),
      kind: sml_namespace::SymbolKind::Structure,
      detail: None,
      idx,
      children,
    })
  }));
  ac.extend(env.ty_env.iter().filter_map(|(name, ty_info)| {
    mvs.clear();
    mvs.extend_for(ty_info.ty_scheme.ty);
    let idx = def_idx(path, ty_info.def?)?;
    Some(DocumentSymbol {
      name: name.as_str().to_owned(),
      kind: sml_namespace::SymbolKind::Type,
      detail: Some(ty_info.ty_scheme.display(mvs, syms).to_string()),
      idx,
      children: Vec::new(),
    })
  }));
  ac.extend(env.val_env.iter().flat_map(|(name, val_info)| {
    mvs.clear();
    mvs.extend_for(val_info.ty_scheme.ty);
    let detail = val_info.ty_scheme.display(mvs, syms).to_string();
    val_info.defs.iter().filter_map(move |&def| {
      let idx = def_idx(path, def)?;
      Some(DocumentSymbol {
        name: name.as_str().to_owned(),
        kind: val_info_symbol_kind(tys, val_info),
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

fn val_info_symbol_kind(tys: &Tys, val_info: &ValInfo) -> sml_namespace::SymbolKind {
  match val_info.id_status {
    IdStatus::Con => sml_namespace::SymbolKind::Constructor,
    IdStatus::Exn(_) => sml_namespace::SymbolKind::Exception,
    IdStatus::Val => match tys.data(val_info.ty_scheme.ty) {
      TyData::Fn(_) => sml_namespace::SymbolKind::Function,
      _ => sml_namespace::SymbolKind::Value,
    },
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

/// A completion item.
#[derive(Debug)]
pub struct CompletionItem {
  /// The label.
  pub label: String,
  /// The kind.
  pub kind: sml_namespace::SymbolKind,
  /// Detail about it.
  pub detail: Option<String>,
  /// Markdown documentation for it.
  pub documentation: Option<String>,
}
