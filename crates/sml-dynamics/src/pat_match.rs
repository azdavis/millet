//! Matching a value against a pattern.

use crate::types::{ConKind, Cx, Val, ValEnv};
use sml_hir::SCon;
use sml_statics_types::info::IdStatus;

pub(crate) fn get(ac: &mut ValEnv, cx: Cx<'_>, pat: sml_hir::PatIdx, val: &Val) -> bool {
  let pat = pat.expect("no pat");
  match (&cx.ars.pat[pat], val) {
    (sml_hir::Pat::Wild, _) => true,
    (sml_hir::Pat::Con(path, pat_arg), _) => match &cx.pat[pat] {
      IdStatus::Con => get_con(ac, cx, path.last(), ConKind::Dat, *pat_arg, val),
      IdStatus::Exn(exn) => get_con(ac, cx, path.last(), ConKind::Exn(*exn), *pat_arg, val),
      IdStatus::Val => {
        assert!(path.prefix().is_empty());
        assert!(pat_arg.is_none());
        ac.insert(path.last().clone(), val.clone());
        true
      }
    },
    (_, Val::Closure { .. } | Val::Builtin(_)) => {
      unreachable!("match non-(Wild or Con) with Closure or Builtin")
    }
    (sml_hir::Pat::SCon(pat_sc), Val::SCon(val_sc)) => match (pat_sc, val_sc) {
      (SCon::Real(_), _) => unreachable!("Real pattern"),
      (_, SCon::Real(_)) => unreachable!("match non-(Wild or Con) with Real"),
      (SCon::Int(pat_int), SCon::Int(val_int)) => pat_int == val_int,
      (SCon::Word(pat_word), SCon::Word(val_word)) => pat_word == val_word,
      (SCon::Char(pat_char), SCon::Char(val_char)) => pat_char == val_char,
      (SCon::String(pat_str), SCon::String(val_str)) => pat_str == val_str,
      (SCon::Int(_) | SCon::Word(_) | SCon::Char(_) | SCon::String(_), _) => {
        unreachable!("SCon types do not match")
      }
    },
    (sml_hir::Pat::SCon(_), Val::Con(_) | Val::Record(_)) => {
      unreachable!("match SCon with (Con or Record")
    }
    (sml_hir::Pat::Record { rows: pat_rows, allows_other: _ }, Val::Record(val_rows)) => {
      pat_rows.iter().all(|(lab, pat)| get(ac, cx, *pat, &val_rows[lab]))
    }
    (sml_hir::Pat::Record { .. }, Val::SCon(_) | Val::Con(_)) => {
      unreachable!("match Record with (SCon or Con")
    }
    (sml_hir::Pat::Typed(pat, _), _) => get(ac, cx, *pat, val),
    (sml_hir::Pat::As(name, pat), val) => {
      ac.insert(name.clone(), val.clone());
      get(ac, cx, *pat, val)
    }
    (sml_hir::Pat::Or(or_pat), val) => {
      let mut or_ac = ValEnv::default();
      for &pat in std::iter::once(&or_pat.first).chain(or_pat.rest.iter()) {
        if !get(&mut or_ac, cx, pat, val) {
          or_ac.clear();
          continue;
        }
        for (name, val) in or_ac {
          assert!(ac.insert(name, val.clone()).is_none());
        }
        return true;
      }
      false
    }
    (sml_hir::Pat::Vector(_), _) => todo!(),
  }
}

fn get_con(
  ac: &mut ValEnv,
  cx: Cx<'_>,
  name: &str_util::Name,
  kind: ConKind,
  pat_arg: Option<sml_hir::PatIdx>,
  val: &Val,
) -> bool {
  let con = match val {
    Val::Con(x) => x,
    _ => unreachable!("match Con with non-Con"),
  };
  let same_con = match (kind, &con.kind) {
    (ConKind::Dat, ConKind::Dat) => *name == con.name,
    (ConKind::Exn(e1), ConKind::Exn(e2)) => e1 == *e2,
    (ConKind::Dat, ConKind::Exn(_)) | (ConKind::Exn(_), ConKind::Dat) => false,
  };
  if !same_con {
    return false;
  }
  match (pat_arg, &con.arg) {
    (None, None) => true,
    (Some(pat_arg), Some(val_arg)) => get(ac, cx, pat_arg, val_arg.as_ref()),
    (Some(_), None) => unreachable!("pat Con has arg but val does not"),
    (None, Some(_)) => unreachable!("pat Con has no arg but val does"),
  }
}
