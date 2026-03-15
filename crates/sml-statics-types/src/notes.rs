//! Notes when unifying. MUST be COMPLETELY UNNECESSARY from a correctness perspective.

use crate::ty::{Ty, TyKind, Tys, UnresolvedRecordMetaTyVar};
use crate::{St, display, overload};
use core::fmt;
use fast_hash::{FxHashMap, FxHashSet};
use std::collections::{BTreeMap, BTreeSet};
use std::collections::{btree_map, hash_map};

/// The notes when unifying.
#[derive(Debug, Default, Clone)]
pub struct Notes {
  idx: Option<sml_hir::Idx>,
  introductions: FxHashMap<idx::Idx, sml_hir::Idx>,
  solutions: FxHashMap<idx::Idx, Solution>,
  back_edges: FxHashMap<idx::Idx, Vec<Solution>>,
  overloads: FxHashMap<idx::Idx, Vec<(sml_hir::Idx, overload::Overload)>>,
  records: FxHashMap<idx::Idx, Vec<(sml_hir::Idx, UnresolvedRecordMetaTyVar)>>,
}

impl Notes {
  /// Clears this of all entries.
  pub fn clear(&mut self) {
    self.introductions.clear();
    self.solutions.clear();
    self.back_edges.clear();
    self.overloads.clear();
    self.records.clear();
    self.exit();
  }

  /// Enter a unification with the idx.
  pub fn enter(&mut self, idx: Option<sml_hir::Idx>) {
    self.idx = idx;
  }

  /// Exit the unification.
  pub fn exit(&mut self) {
    self.idx = None;
  }

  /// Note where a meta var was introduced.
  pub fn introduce(&mut self, mv: Ty, idx: sml_hir::Idx) {
    debug_assert_eq!(mv.kind, TyKind::MetaVar);
    debug_assert!(self.introductions.insert(mv.idx, idx).is_none());
  }

  /// Note where an overloaded meta var was introduced.
  pub fn introduce_overloaded(&mut self, mv: Ty, idx: sml_hir::Idx, ov: overload::Overload) {
    self.introduce(mv, idx);
    self.overloads.entry(mv.idx).or_default().push((idx, ov));
  }

  pub(crate) fn solve(&mut self, mv: Ty, ty: Ty) {
    debug_assert_eq!(mv.kind, TyKind::MetaVar);
    let Some(idx) = self.idx else { return };
    debug_assert!(self.solutions.insert(mv.idx, Solution { idx, ty }).is_none());
    if ty.kind == TyKind::MetaVar {
      self.back_edges.entry(ty.idx).or_default().push(Solution { idx, ty: mv });
    }
  }

  pub(crate) fn overload(&mut self, mv: Ty, ov: overload::Overload) {
    debug_assert_eq!(mv.kind, TyKind::MetaVar);
    let Some(idx) = self.idx else { return };
    self.overloads.entry(mv.idx).or_default().push((idx, ov));
  }

  pub(crate) fn record(&mut self, mv: Ty, ur: UnresolvedRecordMetaTyVar) {
    debug_assert_eq!(mv.kind, TyKind::MetaVar);
    let Some(idx) = self.idx else { return };
    self.records.entry(mv.idx).or_default().push((idx, ur));
  }

  /// Get related types.
  #[must_use]
  pub fn get_events(&self, tys: &Tys, mut work: Vec<Ty>) -> Events {
    let mut ret = Events::default();
    let mut seen_tys = FxHashSet::<Ty>::default();
    let mut next = 0u32;
    while let Some(ty) = work.pop() {
      if !seen_tys.insert(ty) {
        continue;
      }
      tys.meta_vars(ty, &mut |meta_var| {
        let mv_seen_idx = match ret.meta_vars.entry(meta_var.idx) {
          hash_map::Entry::Occupied(entry) => *entry.get(),
          hash_map::Entry::Vacant(entry) => {
            let ret = *entry.insert(next);
            next += 1;
            ret
          }
        };
        if let Some(&idx) = self.introductions.get(&meta_var.idx) {
          ret.groups.entry(idx).or_default().introductions.insert(mv_seen_idx);
        }
        for &(idx, ov) in self.overloads.get(&meta_var.idx).into_iter().flatten() {
          match ret.groups.entry(idx).or_default().overloads.entry(mv_seen_idx) {
            btree_map::Entry::Occupied(mut entry) => match entry.get().unify(ov) {
              Some(x) => {
                entry.insert(x);
              }
              None => debug_assert!(false, "overload unify failed: {} {}", entry.get(), ov),
            },
            btree_map::Entry::Vacant(entry) => {
              entry.insert(ov);
            }
          }
        }
        for &(idx, ref ur) in self.records.get(&meta_var.idx).into_iter().flatten() {
          let records = ret.groups.entry(idx).or_default().records.entry(mv_seen_idx).or_default();
          records.push(ur.clone());
        }
        for solution in self
          .back_edges
          .get(&meta_var.idx)
          .into_iter()
          .flatten()
          .chain(self.solutions.get(&meta_var.idx))
        {
          ret.groups.entry(solution.idx).or_default().solutions.insert(mv_seen_idx, solution.ty);
          work.push(solution.ty);
        }
      });
    }
    ret
  }
}

#[derive(Debug, Clone)]
struct Solution {
  idx: sml_hir::Idx,
  ty: Ty,
}

/// Unification things that happened at indices.
#[derive(Debug, Default)]
pub struct Events {
  groups: BTreeMap<sml_hir::Idx, IdxEvents>,
  meta_vars: FxHashMap<idx::Idx, u32>,
}

impl Events {
  /// Returns the things inside.
  pub fn iter(&self) -> impl Iterator<Item = EventGroup<'_>> {
    self.groups.iter().map(|(&idx, events)| EventGroup { idx, events })
  }

  /// The named types.
  #[must_use]
  pub fn named_tys(&self) -> NamedTys<'_> {
    NamedTys(&self.meta_vars)
  }
}

/// Names for types.
#[derive(Debug, Clone, Copy)]
pub struct NamedTys<'a>(&'a FxHashMap<idx::Idx, u32>);

impl NamedTys<'_> {
  /// Returns if this empty.
  #[must_use]
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  /// Maybe name a type.
  #[must_use]
  pub fn name(&self, ty: Ty) -> Option<impl fmt::Display> {
    if ty.kind != TyKind::MetaVar {
      return None;
    }
    self.0.get(&ty.idx).map(|&x| NumberedMetaVar(x))
  }
}

struct NumberedMetaVar(u32);

impl fmt::Display for NumberedMetaVar {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "?{}", self.0)
  }
}

/// A group of events for an idx.
#[derive(Debug)]
pub struct EventGroup<'a> {
  idx: sml_hir::Idx,
  events: &'a IdxEvents,
}

impl<'a> EventGroup<'a> {
  /// Returns the idx for this.
  #[must_use]
  pub fn idx(&self) -> sml_hir::Idx {
    self.idx
  }

  /// Returns a value that displays this.
  #[must_use]
  pub fn display(&'a self, st: &'a St) -> impl fmt::Display {
    IdxEventsDisplay { events: self.events, st }
  }
}

#[derive(Debug, Default)]
struct IdxEvents {
  introductions: BTreeSet<u32>,
  overloads: BTreeMap<u32, overload::Overload>,
  records: BTreeMap<u32, Vec<UnresolvedRecordMetaTyVar>>,
  solutions: BTreeMap<u32, Ty>,
}

impl IdxEvents {
  fn count(&self) -> usize {
    self.introductions.len() + self.overloads.len() + self.records.len() + self.solutions.len()
  }
}

struct IdxEventsDisplay<'a> {
  events: &'a IdxEvents,
  st: &'a St,
}

impl fmt::Display for IdxEventsDisplay<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let count = self.events.count();
    let prefix = if count > 1 {
      writeln!(f, "{count} events:")?;
      "- "
    } else {
      ""
    };
    for mv in &self.events.introductions {
      writeln!(f, "{prefix}introduce ?{mv}")?;
    }
    for (mv, ov) in &self.events.overloads {
      writeln!(f, "{prefix}overload ?{mv} to `{ov}`")?;
    }
    for (mv, urs) in &self.events.records {
      for ur in urs {
        let ur = display::record_meta_var(self.st, &ur.rows, config::DiagnosticLines::One);
        writeln!(f, "{prefix}narrow ?{mv} to `{ur}`")?;
      }
    }
    for (mv, sol) in &self.events.solutions {
      let sol = sol.display(self.st, config::DiagnosticLines::One);
      writeln!(f, "{prefix}solve ?{mv} to `{sol}`")?;
    }
    Ok(())
  }
}
