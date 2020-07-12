//! Ensuring a sequence of patterns is completely and non-redundantly exhaustive.
//!
//! Adapted from 'ML pattern match compilation and partial evaluation' by Peter Sestoft.
//!
//! One who both has read his paper and this implementation will notice some differences between the
//! two:
//!
//! - We don't compute an explicit decision tree because we're not actually compiling anything.
//! - We don't record access information because of the same.
//! - We do keep track of matched patterns with a set of Locs because we want to report which
//!   pattern(s) are unreachable.
//! - We switch around the order of some work lists (vectors) for efficiency.
//! - We reorganize the types used to encode invariants (instead of having two lists which ought to
//!   be the same length, have a single list of structs with two fields).
//! - We have a few more options for Con because we don't want to represent things like numbers as
//!   strings.

use crate::loc::{Loc, Located};
use crate::statics::types::{Con, Error, Pat, Result, Span};
use std::collections::HashSet;

/// Returns `Ok(())` iff the pats are exhaustive and not redundant.
pub fn ck_match(pats: Vec<Located<Pat>>, loc: Loc) -> Result<()> {
  match ck(pats) {
    Res::Exhaustive => Ok(()),
    Res::NonExhaustive => Err(loc.wrap(Error::NonExhaustiveMatch)),
    Res::Unreachable(loc) => Err(loc.wrap(Error::UnreachablePattern)),
  }
}

/// Returns `Ok(())` iff the singular pat is exhaustive.
pub fn ck_bind(pat: Pat, loc: Loc) -> Result<()> {
  match ck(vec![loc.wrap(pat)]) {
    Res::Exhaustive => Ok(()),
    Res::NonExhaustive => Err(loc.wrap(Error::NonExhaustiveBinding)),
    Res::Unreachable(_) => unreachable!(),
  }
}

/// Returns `Ok(())` iff the pats are not redundant.
pub fn ck_handle(pats: Vec<Located<Pat>>) -> Result<()> {
  match ck(pats) {
    Res::Exhaustive | Res::NonExhaustive => Ok(()),
    Res::Unreachable(loc) => Err(loc.wrap(Error::UnreachablePattern)),
  }
}

/// A description of an object being matched (the "match head"). We use this to cumulatively record
/// information about the match head as we see more patterns.
#[derive(Clone)]
enum Desc {
  /// We know that the match head is this Con, and the arguments to that Con are described.
  Pos(Con, Vec<Desc>),
  /// We know that the match head is not any of these Con.
  Neg(Vec<Con>),
}

/// The return from `static_match`.
enum StaticMatch {
  /// The Con is consistent with the Desc.
  Yes,
  /// The Con is not consistent with the Desc.
  No,
  /// The Con might be consistent with the Desc. If this is returned, then the Desc was Neg.
  Maybe(Vec<Con>),
}

/// An item in the work list.
#[derive(Clone)]
struct WorkItem {
  /// The constructor.
  con: Con,
  /// Descriptions about the processed arguments to the constructor, in the actual order of the
  /// arguments. We process the arguments left to right.
  descs: Vec<Desc>,
  /// The un-processed arguments in reverse order. These are backwards, so the first one is the
  /// rightmost argument in the source.
  args: Vec<Arg>,
}

/// An un-processed argument.
#[derive(Clone)]
struct Arg {
  /// The pattern.
  pat: Pat,
  /// A description about the pattern, possibly computed by analyzing previous patterns in the
  /// match.
  desc: Desc,
}

/// The work list. The back of the list is the next item to be processed (it's a stack).
type Work = Vec<WorkItem>;

/// The context, passed along through most of the main functions. This is an set of the locations of
/// the patterns of the match. As we determine a pattern is reachable, we remove its `Loc` from this
/// set. At the end, the set contains the locations of all unreachable patterns.
type Cx = HashSet<Loc>;

/// The patterns, created from an `into_iter()` call on the passed-in `Vec<Located<Pat>>`.
type Pats = std::vec::IntoIter<Located<Pat>>;

/// A determination of what the patterns were.
enum Res {
  /// They were exhaustive.
  Exhaustive,
  /// They were not exhaustive.
  NonExhaustive,
  /// There was a pattern which can never be reached.
  Unreachable(Loc),
}

/// The main function, which the exported functions ultimately call.
fn ck(pats: Vec<Located<Pat>>) -> Res {
  let mut cx: Cx = pats.iter().map(|x| x.loc).collect();
  if fail(&mut cx, Desc::Neg(vec![]), pats.into_iter()) {
    // Must choose the minimum loc to get the first unreachable pattern.
    match cx.into_iter().min() {
      None => Res::Exhaustive,
      Some(loc) => Res::Unreachable(loc),
    }
  } else {
    Res::NonExhaustive
  }
}

/// Augment the last element in the work list with a new Desc.
fn augment(mut work: Work, d: Desc) -> Work {
  if let Some(item) = work.last_mut() {
    item.descs.push(d);
  }
  work
}

/// Builds a `Desc` from a base `Desc` and a work list.
fn build_desc(mut d: Desc, work: Work) -> Desc {
  // Since we take the from the end of `work`, reverse the iterator.
  for item in work.into_iter().rev() {
    // First the computed descriptions.
    let mut descs = item.descs;
    // Then this description.
    descs.push(d);
    // Then the argument descriptions. We reverse because these are stored in reverse, so reversing
    // again will straighten it out.
    descs.append(&mut item.args.into_iter().rev().map(|x| x.desc).collect());
    d = Desc::Pos(item.con, descs)
  }
  d
}

/// Statically match a `Con` against a `Desc`.
fn static_match(con: Con, d: &Desc) -> StaticMatch {
  match d {
    Desc::Pos(c, _) => {
      if *c == con {
        StaticMatch::Yes
      } else {
        StaticMatch::No
      }
    }
    Desc::Neg(cons) => {
      if cons.iter().any(|c| c == &con) {
        StaticMatch::No
      } else if con.span() == Span::Finite(cons.len() + 1) {
        // This is the last con.
        StaticMatch::Yes
      } else {
        StaticMatch::Maybe(cons.clone())
      }
    }
  }
}

/// Tries to pass the next pattern in `pats` to a fresh call to `do_match`. Returns whether the
/// match was exhaustive.
fn fail(cx: &mut Cx, d: Desc, mut pats: Pats) -> bool {
  match pats.next() {
    None => false,
    Some(pat) => do_match(cx, pat, d, vec![], pats),
  }
}

/// Tries to prove a pat located at the `Loc` is reachable. Removes the `Loc` from the `Cx` if it
/// can prove this. Returns whether the match was exhaustive.
fn succeed(cx: &mut Cx, loc: Loc, mut work: Work, pats: Pats) -> bool {
  match work.pop() {
    None => {
      cx.remove(&loc);
      true
    }
    Some(mut item) => match item.args.pop() {
      None => {
        let work = augment(work, Desc::Pos(item.con, item.descs));
        succeed(cx, loc, work, pats)
      }
      Some(arg) => {
        work.push(item);
        do_match(cx, loc.wrap(arg.pat), arg.desc, work, pats)
      }
    },
  }
}

/// Updates the work list with new work for the pattern at the `Loc`, then continues on to
/// `succeed`. Returns whether the match was exhaustive.
fn succeed_with(
  cx: &mut Cx,
  loc: Loc,
  mut work: Work,
  con: Con,
  args: Vec<Pat>,
  d: Desc,
  pats: Pats,
) -> bool {
  let arg_descs = match d {
    Desc::Neg(_) => args.iter().map(|_| Desc::Neg(vec![])).collect(),
    Desc::Pos(_, descs) => descs,
  };
  assert_eq!(args.len(), arg_descs.len());
  work.push(WorkItem {
    con,
    descs: vec![],
    args: args
      .into_iter()
      .zip(arg_descs)
      .rev()
      .map(|(pat, desc)| Arg { pat, desc })
      .collect(),
  });
  succeed(cx, loc, work, pats)
}

/// Tries to match the `Pat` against the `Desc` using the other helpers. Returns whether the match
/// was exhaustive.
fn do_match(cx: &mut Cx, pat: Located<Pat>, d: Desc, work: Work, pats: Pats) -> bool {
  match pat.val {
    Pat::Anything => succeed(cx, pat.loc, augment(work, d), pats),
    Pat::Con(con, args) => match static_match(con, &d) {
      StaticMatch::Yes => succeed_with(cx, pat.loc, work, con, args, d, pats),
      StaticMatch::No => fail(cx, build_desc(d, work), pats),
      StaticMatch::Maybe(mut cons) => {
        cons.push(con);
        // TODO avoid these clones?
        succeed_with(cx, pat.loc, work.clone(), con, args, d, pats.clone())
          && fail(cx, build_desc(Desc::Neg(cons), work), pats)
      }
    },
  }
}
