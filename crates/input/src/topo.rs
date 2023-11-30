//! Run a topo sort on a sequence of `BasDec`s at paths.

use paths::PathId;
use std::collections::BTreeSet;

pub(crate) fn check<'a, I>(iter: I) -> Result<(), topo_sort::CycleError<PathId>>
where
  I: Iterator<Item = (PathId, &'a mlb_hir::BasDec)>,
{
  let graph: topo_sort::Graph<_> = iter
    .map(|(path, bas_dec)| {
      let mut ac = BTreeSet::<PathId>::new();
      bas_dec_paths(&mut ac, bas_dec);
      (path, ac)
    })
    .collect();
  topo_sort::get(&graph)?;
  Ok(())
}

fn bas_dec_paths(ac: &mut BTreeSet<PathId>, dec: &mlb_hir::BasDec) {
  match dec {
    mlb_hir::BasDec::Open(_) | mlb_hir::BasDec::Export(_, _, _) => {}
    mlb_hir::BasDec::Path(p, _) => {
      ac.insert(*p);
    }
    mlb_hir::BasDec::SourcePathSet(paths) => ac.extend(paths.iter().map(|&(x, _)| x)),
    mlb_hir::BasDec::Basis(_, exp) => bas_exp_paths(ac, exp),
    mlb_hir::BasDec::Local(local_dec, in_dec) => {
      bas_dec_paths(ac, local_dec);
      bas_dec_paths(ac, in_dec);
    }
    mlb_hir::BasDec::Ann(_, dec) => bas_dec_paths(ac, dec),
    mlb_hir::BasDec::Seq(decs) => {
      for dec in decs {
        bas_dec_paths(ac, dec);
      }
    }
  }
}

fn bas_exp_paths(ac: &mut BTreeSet<PathId>, exp: &mlb_hir::BasExp) {
  match exp {
    mlb_hir::BasExp::Bas(dec) => bas_dec_paths(ac, dec),
    mlb_hir::BasExp::Name(_) => {}
    mlb_hir::BasExp::Let(dec, exp) => {
      bas_dec_paths(ac, dec);
      bas_exp_paths(ac, exp);
    }
  }
}
