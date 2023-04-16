//! See [`St`].

use crate::error::{Error, ErrorKind};
use crate::info::Info;
use crate::pat_match::{self, Pat};
use fast_hash::FxHashSet;
use sml_statics_types::ty::{Ty, Tys};
use sml_statics_types::{def, item::Item, mode::Mode, sym::Syms};

/// The mutable state.
///
/// Usually I call this `Cx` but the Definition defines a 'Context' already.
#[derive(Debug)]
pub(crate) struct St {
  pub(crate) info: Info,
  pub(crate) syms: Syms,
  pub(crate) tys: Tys,
  errors: Vec<Error>,
  matches: Vec<Match>,
  /// a subset of all things that have definition sites. currently, only local value variables to a
  /// function.
  defined: Vec<(sml_hir::Idx, str_util::Name)>,
  used: FxHashSet<sml_hir::Idx>,
  /// for making fully qualified names.
  cur_prefix: Vec<str_util::Name>,
}

impl St {
  pub(crate) fn new(mode: Mode, syms: Syms, tys: Tys) -> Self {
    Self {
      info: Info::new(mode),
      tys,
      syms,
      errors: Vec::new(),
      matches: Vec::new(),
      defined: Vec::new(),
      used: FxHashSet::default(),
      cur_prefix: Vec::new(),
    }
  }

  pub(crate) fn def(&self, idx: sml_hir::Idx) -> Option<def::Def> {
    let path = match self.info.mode {
      Mode::Regular(p) => def::Path::Regular(p?),
      Mode::BuiltinLib(p) => def::Path::BuiltinLib(p),
      Mode::PathOrder => return None,
    };
    Some(def::Def::Path(path, idx))
  }

  pub(crate) fn err<I>(&mut self, idx: I, kind: ErrorKind)
  where
    I: Into<sml_hir::Idx>,
  {
    match (self.info.mode, &kind) {
      (Mode::PathOrder, ErrorKind::Undefined(Item::Struct | Item::Sig | Item::Functor, _))
      | (Mode::Regular(_) | Mode::BuiltinLib(_), _) => {
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

  pub(crate) fn insert_case(&mut self, idx: sml_hir::Idx, pats: Vec<Pat>, want: Ty) {
    if self.info.mode.is_path_order() {
      return;
    }
    self.matches.push(Match { kind: MatchKind::Case(pats), want, idx });
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

  pub(crate) fn mk_path(&self, last: str_util::Name) -> sml_path::Path {
    sml_path::Path::new(self.cur_prefix.iter().cloned(), last)
  }

  pub(crate) fn finish(mut self) -> (Syms, Tys, Vec<Error>, Info) {
    if !self.info.mode.is_path_order() {
      for m in std::mem::take(&mut self.matches) {
        match m.kind {
          MatchKind::Bind(pat) => {
            let missing = get_match(&mut self.errors, &self.syms, &mut self.tys, vec![pat], m.want);
            if !missing.is_empty() {
              self.err(m.idx, ErrorKind::NonExhaustiveBinding(missing));
            }
          }
          MatchKind::Case(pats) => {
            let missing = get_match(&mut self.errors, &self.syms, &mut self.tys, pats, m.want);
            if !missing.is_empty() {
              self.err(m.idx, ErrorKind::NonExhaustiveCase(missing));
            }
          }
          MatchKind::Handle(pats) => {
            get_match(&mut self.errors, &self.syms, &mut self.tys, pats, m.want);
          }
        }
      }
      for (idx, name) in std::mem::take(&mut self.defined) {
        if !self.used.contains(&idx) {
          self.err(idx, ErrorKind::Unused(Item::Val, name));
        }
      }
    }
    (self.syms, self.tys, self.errors, self.info)
  }
}

/// returns the missing pats.
fn get_match(
  errors: &mut Vec<Error>,
  syms: &Syms,
  tys: &mut Tys,
  pats: Vec<Pat>,
  ty: Ty,
) -> Vec<Pat> {
  let ck = elapsed::log("pattern_match::check", || {
    let mut cx = pat_match::Cx { syms, tys };
    pattern_match::check::<pat_match::Lang>(&mut cx, pats, ty)
  });
  let ck = match ck {
    Ok(x) => x,
    // we already should have emitted other errors in this case.
    Err(_) => return Vec::new(),
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
