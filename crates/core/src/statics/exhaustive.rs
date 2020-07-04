//! Ensuring a sequence of patterns completely exhausts a type non-redundantly.

use crate::ast::Label;
use crate::intern::StrRef;
use crate::loc::Located;
use crate::statics::types::{Datatypes, IdStatus, Pat, Result, StaticsError, Ty};
use std::collections::{HashMap, HashSet};

/// Note that `ty` _must_ be the unification of `pats`. That is, each pat in `pats` must have been
/// derived from a call to `ck_pat`, which returned both a Pat and a Ty. The Pat should be in
/// `pats`, and the Ty should be unified `ty`. Additionally, after having unified all the pattern
/// Tys, `ty` must have the resulting Subst applied to it. A better API design might have this
/// function accept only a vector of AST `Pat`, compute the statics `Pat`s and their types, unify
/// the types, apply the Subst, and _then_ proceed as we do here.
pub fn ck(dts: &Datatypes, ty: &Ty, pats: Vec<Located<Pat>>) -> Result<bool> {
  let mut needs = needs_from_ty(&dts, &ty);
  for pat in pats {
    let mut new_needs = Vec::with_capacity(needs.len());
    // NOTE here, removing is considered a change.
    let mut changed = false;
    for need in needs {
      match ck_need(need, &pat.val, dts) {
        NeedRes::Removed => changed = true,
        NeedRes::Changed(mut ns) => {
          changed = true;
          new_needs.append(&mut ns);
        }
        NeedRes::Unchanged(n) => new_needs.push(n),
      }
    }
    if !changed {
      return Err(pat.loc.wrap(StaticsError::UnreachablePattern));
    }
    needs = new_needs;
  }
  Ok(needs.is_empty())
}

/// Int, Word, String, Char each have a set of every element of this type we DO NOT need
#[derive(Debug, Clone)]
enum Need {
  Unmatchable,
  Int(HashSet<i32>),
  Word(HashSet<i32>),
  String(HashSet<StrRef>),
  Char(HashSet<u8>),
  Record(HashMap<Label, Need>),
  Ctor(StrRef, Option<Box<ArgNeed>>),
}

#[derive(Debug, Clone)]
enum ArgNeed {
  // TODO could avoid storing the type in here if we carried ty along in ck_need.
  Lazy(Ty),
  Forced(Need),
}

fn needs_from_ty(dts: &Datatypes, ty: &Ty) -> Vec<Need> {
  match ty {
    Ty::Var(_) | Ty::Arrow(_, _) => vec![Need::Unmatchable],
    Ty::Record(rows) => {
      let rows = rows
        .iter()
        .map(|&(lab, ref ty)| (lab, needs_from_ty(dts, ty)))
        .collect();
      cross(rows)
        .into_iter()
        .map(|xs| Need::Record(xs.into_iter().collect()))
        .collect()
    }
    Ty::Ctor(args, sym) => {
      if *ty == Ty::INT {
        return vec![Need::Int(HashSet::new())];
      }
      if *ty == Ty::WORD {
        return vec![Need::Word(HashSet::new())];
      }
      if *ty == Ty::STRING {
        return vec![Need::String(HashSet::new())];
      }
      if *ty == Ty::CHAR {
        return vec![Need::Char(HashSet::new())];
      }
      if *ty == Ty::REAL {
        return vec![Need::Unmatchable];
      }
      dts
        .get(sym)
        .unwrap()
        .val_env
        .iter()
        .map(|(&name, val_info)| {
          assert!(matches!(val_info.id_status, IdStatus::Ctor));
          let arg = match val_info.ty_scheme.apply_args(args.clone()) {
            Ty::Arrow(arg_ty, _) => Some(ArgNeed::Lazy(*arg_ty).into()),
            _ => None,
          };
          Need::Ctor(name, arg)
        })
        .collect()
    }
  }
}

#[derive(Debug)]
enum NeedRes {
  Removed,
  /// requires !vec.is_empty()
  Changed(Vec<Need>),
  Unchanged(Need),
}

macro_rules! need_prim {
  ($need: expr, $elem: expr, $name: ident) => {{
    let mut set = match $need {
      Need::$name(set) => set,
      _ => unreachable!(),
    };
    if set.insert($elem) {
      NeedRes::Changed(vec![Need::$name(set)])
    } else {
      NeedRes::Unchanged(Need::$name(set))
    }
  }};
}

fn ck_need(need: Need, pat: &Pat, dts: &Datatypes) -> NeedRes {
  match pat {
    Pat::Anything => NeedRes::Removed,
    Pat::Int(x) => need_prim!(need, *x, Int),
    Pat::Word(x) => need_prim!(need, *x, Word),
    Pat::String(x) => need_prim!(need, *x, String),
    Pat::Char(x) => need_prim!(need, *x, Char),
    Pat::Record(got_rows) => {
      let need_rows = match need {
        Need::Record(xs) => xs,
        _ => unreachable!(),
      };
      let mut new_need_rows = Vec::with_capacity(got_rows.len());
      for (lab, got) in got_rows {
        let need = need_rows.get(&lab).unwrap().clone();
        let ns = match ck_need(need.clone(), got, dts) {
          NeedRes::Removed => vec![],
          NeedRes::Changed(ns) => ns,
          NeedRes::Unchanged(_) => return NeedRes::Unchanged(Need::Record(need_rows)),
        };
        new_need_rows.push((*lab, ns));
      }
      if new_need_rows.iter().all(|(_, ns)| ns.is_empty()) {
        return NeedRes::Removed;
      }
      let mut ret = Vec::new();
      for (lab, needs) in new_need_rows {
        for need in needs {
          let mut add = need_rows.clone();
          assert!(add.insert(lab, need).is_some());
          ret.push(Need::Record(add));
        }
      }
      NeedRes::Changed(ret)
    }
    Pat::Ctor(pat_name, pat_arg) => {
      let (need_name, need_arg) = match need {
        Need::Ctor(x, y) => (x, y),
        _ => unreachable!(),
      };
      if need_name != *pat_name {
        return NeedRes::Unchanged(Need::Ctor(need_name, need_arg));
      }
      let (need, got) = match (need_arg, pat_arg.as_deref()) {
        (None, None) => return NeedRes::Removed,
        (Some(need), Some(got)) => (need, got),
        _ => unreachable!(),
      };
      let (needs, mut changed) = match *need {
        ArgNeed::Lazy(ty) => (needs_from_ty(dts, &ty), true),
        ArgNeed::Forced(need) => (vec![need], false),
      };
      let mut new_needs = Vec::new();
      for need in needs {
        let ns = match ck_need(need, got, dts) {
          NeedRes::Removed => Vec::new(),
          NeedRes::Changed(ns) => {
            changed = true;
            ns
          }
          NeedRes::Unchanged(n) => vec![n],
        };
        let mut ns = ns
          .into_iter()
          .map(|n| Need::Ctor(need_name, Some(ArgNeed::Forced(n).into())))
          .collect();
        new_needs.append(&mut ns);
      }
      if new_needs.is_empty() {
        NeedRes::Removed
      } else if changed {
        NeedRes::Changed(new_needs)
      } else {
        assert_eq!(new_needs.len(), 1);
        NeedRes::Unchanged(new_needs.pop().unwrap())
      }
    }
  }
}

// TODO make more generic (iterator) or output Vec<HashMap<T, U>> to avoid another into_iter the one
// place we use this?
fn cross<T, U>(mut xs: Vec<(T, Vec<U>)>) -> Vec<Vec<(T, U)>>
where
  T: Clone,
  U: Clone,
{
  match xs.pop() {
    None => Vec::new(),
    Some((t, us)) => {
      if xs.is_empty() {
        return us.into_iter().map(|u| vec![(t.clone(), u)]).collect();
      }
      let cross_xs = cross(xs);
      let mut ret = Vec::with_capacity(us.len() * cross_xs.len());
      for u in us {
        for mut ys in cross_xs.clone() {
          ys.push((t.clone(), u.clone()));
          ret.push(ys);
        }
      }
      ret
    }
  }
}

#[cfg(test)]
mod tests {
  type T = &'static str;
  type U = usize;

  fn cross(xs: Vec<(T, Vec<U>)>) -> Vec<Vec<(T, U)>> {
    super::cross(xs)
  }

  #[test]
  fn test_cross() {
    assert_eq!(cross(vec![]), Vec::<Vec<(T, U)>>::new());
    assert_eq!(
      cross(vec![("foo", vec![1, 2, 3])]),
      vec![vec![("foo", 1)], vec![("foo", 2)], vec![("foo", 3)]],
    );
    assert_eq!(
      cross(vec![("foo", vec![1, 2, 3]), ("nope", vec![])]),
      Vec::<Vec<(T, U)>>::new(),
    );
    assert_eq!(
      cross(vec![("foo", vec![1, 2, 3]), ("bar", vec![3, 4, 5, 6])]),
      vec![
        vec![("foo", 1), ("bar", 3)],
        vec![("foo", 2), ("bar", 3)],
        vec![("foo", 3), ("bar", 3)],
        vec![("foo", 1), ("bar", 4)],
        vec![("foo", 2), ("bar", 4)],
        vec![("foo", 3), ("bar", 4)],
        vec![("foo", 1), ("bar", 5)],
        vec![("foo", 2), ("bar", 5)],
        vec![("foo", 3), ("bar", 5)],
        vec![("foo", 1), ("bar", 6)],
        vec![("foo", 2), ("bar", 6)],
        vec![("foo", 3), ("bar", 6)],
      ]
    );
  }
}
