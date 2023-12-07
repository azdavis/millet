//! See [`St`].

use crate::compatible;
use crate::error::{Error, ErrorKind};
use crate::info::Info;
use crate::pat_match::{self, Pat};
use fast_hash::FxHashSet;
use sml_statics_types::ty::{Ty, TyScheme};
use sml_statics_types::{def, item::Item, mode::Mode};

/// The mutable state.
#[derive(Debug)]
pub(crate) struct St<'a> {
  pub(crate) info: Info,
  pub(crate) syms_tys: &'a mut sml_statics_types::St,
  errors: Vec<Error>,
  matches: Vec<Match>,
  eta_reduce: sml_hir::la_arena::ArenaMap<sml_hir::la_arena::Idx<sml_hir::Exp>, str_util::Name>,
  /// a subset of all things that have definition sites. currently, only local value variables to a
  /// function.
  defined: Vec<(sml_hir::Idx, str_util::Name)>,
  used: FxHashSet<sml_hir::Idx>,
  /// for making fully qualified names.
  cur_prefix: Vec<str_util::Name>,
  shadows: Vec<(sml_hir::Idx, str_util::Name, TyScheme, TyScheme)>,
  pub(crate) exp_id_statuses: sml_statics_types::info::IdStatusMap<sml_hir::Exp>,
  pub(crate) pat_id_statuses: sml_statics_types::info::IdStatusMap<sml_hir::Pat>,
}

impl<'a> St<'a> {
  pub(crate) fn new(mode: Mode, syms_tys: &'a mut sml_statics_types::St) -> St<'a> {
    Self {
      info: Info::new(mode),
      syms_tys,
      errors: Vec::new(),
      matches: Vec::new(),
      eta_reduce: sml_hir::la_arena::ArenaMap::default(),
      defined: Vec::new(),
      used: FxHashSet::default(),
      cur_prefix: Vec::new(),
      shadows: Vec::new(),
      exp_id_statuses: sml_statics_types::info::IdStatusMap::default(),
      pat_id_statuses: sml_statics_types::info::IdStatusMap::default(),
    }
  }

  pub(crate) fn def_opt(&self, idx: sml_hir::Idx) -> Option<def::Def> {
    let path = match self.info.mode {
      Mode::Regular(Some(p)) => def::Path::Regular(p),
      Mode::BuiltinLib(p) => def::Path::BuiltinLib(p),
      Mode::Regular(None) | Mode::PathOrder | Mode::Dynamics => return None,
    };
    Some(def::Def::Path(path, idx))
  }

  pub(crate) fn def(&self, idx: sml_hir::Idx) -> FxHashSet<def::Def> {
    self.def_opt(idx).into_iter().collect()
  }

  pub(crate) fn err<I>(&mut self, idx: I, kind: ErrorKind)
  where
    I: Into<sml_hir::Idx>,
  {
    match (self.info.mode, &kind) {
      (Mode::PathOrder, ErrorKind::Undefined(Item::Struct | Item::Sig | Item::Functor, _))
      | (Mode::Regular(_) | Mode::BuiltinLib(_) | Mode::Dynamics, _) => {
        self.errors.push(Error { idx: idx.into(), kind });
      }
      (Mode::PathOrder, _) => {}
    }
  }

  pub(crate) fn insert_bind(&mut self, idx: sml_hir::Idx, pat: Pat, want: Ty) {
    if self.info.mode.is_path_order() {
      return;
    }
    self.matches.push(Match { kind: MatchKind::Bind(pat), want, idx });
  }

  pub(crate) fn insert_handle(&mut self, idx: sml_hir::Idx, pats: Vec<Pat>, want: Ty) {
    if self.info.mode.is_path_order() {
      return;
    }
    self.matches.push(Match { kind: MatchKind::Handle(pats), want, idx });
  }

  pub(crate) fn insert_case(
    &mut self,
    idx: sml_hir::la_arena::Idx<sml_hir::Exp>,
    pats: Vec<Pat>,
    want: Ty,
    eta_reduce: Option<str_util::Name>,
  ) {
    if self.info.mode.is_path_order() {
      return;
    }
    self.matches.push(Match { kind: MatchKind::Case(pats), want, idx: idx.into() });
    if let Some(name) = eta_reduce {
      self.eta_reduce.insert(idx, name);
    }
  }

  pub(crate) fn mark_eta_reduce_unable(&mut self, idx: sml_hir::la_arena::Idx<sml_hir::Exp>) {
    self.eta_reduce.remove(idx);
  }

  pub(crate) fn mark_defined(&mut self, idx: sml_hir::Idx, name: str_util::Name) {
    self.defined.push((idx, name));
  }

  pub(crate) fn mark_used(&mut self, idx: sml_hir::Idx) {
    if self.info.mode.is_path_order() {
      return;
    }
    self.used.insert(idx);
  }

  pub(crate) fn push_prefix(&mut self, name: str_util::Name) {
    if self.info.mode.is_path_order() {
      return;
    }
    self.cur_prefix.push(name);
  }

  pub(crate) fn pop_prefix(&mut self) {
    if self.info.mode.is_path_order() {
      return;
    }
    self.cur_prefix.pop().expect("no matching push_structure");
  }

  // returns whether given the current mode and prefix, a type should be reported unqualified.
  fn is_well_known_prefix(&self) -> bool {
    match self.info.mode {
      Mode::BuiltinLib(_) => {}
      Mode::Regular(_) | Mode::PathOrder | Mode::Dynamics => return false,
    }
    let [prefix] = &self.cur_prefix[..] else { return false };
    matches!(prefix.as_str(), "Option" | "Array" | "Vector")
  }

  pub(crate) fn mk_path(&self, last: str_util::Name) -> sml_path::Path {
    if self.is_well_known_prefix() {
      sml_path::Path::one(last)
    } else {
      sml_path::Path::new(self.cur_prefix.iter().cloned(), last)
    }
  }

  pub(crate) fn finish(&mut self) -> Vec<Error> {
    if !self.info.mode.is_path_order() {
      for m in std::mem::take(&mut self.matches) {
        match m.kind {
          MatchKind::Bind(pat) => {
            let missing = get_match(&mut self.errors, self.syms_tys, vec![pat], m.want);
            if !missing.is_empty() {
              self.err(m.idx, ErrorKind::NonExhaustiveBinding(missing));
            }
          }
          MatchKind::Case(pats) => {
            let missing = get_match(&mut self.errors, self.syms_tys, pats, m.want);
            if !missing.is_empty() {
              self.err(m.idx, ErrorKind::NonExhaustiveCase(missing));
            }
          }
          MatchKind::Handle(pats) => {
            get_match(&mut self.errors, self.syms_tys, pats, m.want);
          }
        }
      }
      for (idx, name) in std::mem::take(&mut self.defined) {
        if !self.used.contains(&idx) {
          self.err(idx, ErrorKind::Unused(Item::Val, name));
        }
      }
      for (idx, name) in std::mem::take(&mut self.eta_reduce) {
        self.err(idx, ErrorKind::CanEtaReduce(name));
      }
      for (idx, name, old, new) in std::mem::take(&mut self.shadows) {
        if compatible::eq_ty_scheme(self, &old, &new).is_ok() {
          self.err(idx, ErrorKind::ShadowInCaseWithSameTy(name, old));
        }
      }
    }
    std::mem::take(&mut self.errors)
  }

  /// NOTE: we use this to defer the compatible check between `old` and `new`, because if we do it
  /// now, we add constraints on meta type variables in `new` that don't make sense.
  pub(crate) fn insert_shadow(
    &mut self,
    idx: sml_hir::Idx,
    name: str_util::Name,
    old: TyScheme,
    new: TyScheme,
  ) {
    assert!(new.bound_vars.is_empty());
    if self.info.mode.is_path_order() {
      return;
    }
    self.shadows.push((idx, name, old, new));
  }
}

/// returns the missing pats.
fn get_match(
  errors: &mut Vec<Error>,
  syms_tys: &mut sml_statics_types::St,
  pats: Vec<Pat>,
  ty: Ty,
) -> Vec<Pat> {
  let ck = elapsed::log("pattern_match::check", || {
    pattern_match::check::<pat_match::Lang>(syms_tys, pats, ty)
  });
  let Ok(ck) = ck else {
    // we already should have emitted other errors in this case.
    return Vec::new();
  };
  let mut unreachable: Vec<_> = ck.unreachable.into_iter().flatten().collect();
  unreachable.sort_unstable_by_key(|x| x.into_raw());
  for idx in unreachable {
    errors.push(Error { idx: idx.into(), kind: ErrorKind::UnreachablePattern });
  }
  ck.missing
}

#[derive(Debug)]
struct Match {
  kind: MatchKind,
  want: Ty,
  idx: sml_hir::Idx,
}

#[derive(Debug)]
enum MatchKind {
  Bind(Pat),
  Case(Vec<Pat>),
  Handle(Vec<Pat>),
}
