//! Lower a MLB root group into a map of source files and parsed groups.

use crate::types::Group;
use crate::util::{
  get_path_id_in_group, read_file, Error, ErrorKind, ErrorSource, GroupPathToProcess, StartedGroup,
};
use fast_hash::FxHashSet;
use paths::{PathId, PathMap};

pub(crate) fn get<F>(
  fs: &F,
  sources: &mut PathMap<String>,
  groups: &mut PathMap<Group>,
  paths: &mut paths::Store,
  path_vars: &slash_var_path::Env,
  path: paths::PathId,
  errors: &mut Vec<Error>,
) where
  F: paths::FileSystem,
{
  let init = GroupPathToProcess { parent: path, range: None, path };
  let mut st = St { fs, paths, sources, stack: vec![init], errors };
  while let Some(cur) = st.stack.pop() {
    if groups.contains_key(&cur.path) {
      continue;
    }
    let group = match StartedGroup::new(st.paths, cur, fs) {
      Ok(x) => x,
      Err(e) => {
        st.errors.push(e.into_error());
        continue;
      }
    };
    let syntax_dec = match mlb_syntax::get(group.contents.as_str(), path_vars) {
      Ok(x) => x,
      Err(e) => {
        st.errors.push(Error::new(
          ErrorSource { path: None, range: group.pos_db.range_utf16(e.text_range()) },
          group.path.as_path().to_owned(),
          ErrorKind::Mlb(e),
        ));
        cov_mark::hit("undefined_path_var_import");
        mlb_syntax::BasDec::Seq(Vec::new())
      }
    };
    let cx = Cx { group, path_id: cur.path };
    let bas_dec = get_bas_dec(&mut st, &cx, syntax_dec);
    groups.insert(cur.path, Group { bas_dec, pos_db: cx.group.pos_db });
  }
}

struct St<'a, F> {
  fs: &'a F,
  paths: &'a mut paths::Store,
  sources: &'a mut PathMap<String>,
  stack: Vec<GroupPathToProcess>,
  errors: &'a mut Vec<Error>,
}

struct Cx {
  group: StartedGroup,
  path_id: PathId,
}

fn get_bas_dec<F>(st: &mut St<'_, F>, cx: &Cx, dec: mlb_syntax::BasDec) -> mlb_hir::BasDec
where
  F: paths::FileSystem,
{
  match dec {
    mlb_syntax::BasDec::Basis(binds) => {
      let mut names = FxHashSet::<str_util::Name>::default();
      let binds: Vec<_> = binds
        .into_iter()
        .map(|(name, exp)| {
          if !names.insert(name.val.clone()) {
            st.errors.push(Error::new(
              ErrorSource { path: None, range: cx.group.pos_db.range_utf16(name.range) },
              cx.group.path.as_path().to_owned(),
              ErrorKind::Duplicate(name.val.clone()),
            ));
          }
          let exp = get_bas_exp(st, cx, exp);
          mlb_hir::BasDec::Basis(name, exp.into())
        })
        .collect();
      mlb_hir::BasDec::seq(binds)
    }
    mlb_syntax::BasDec::Open(names) => {
      mlb_hir::BasDec::seq(names.into_iter().map(mlb_hir::BasDec::Open).collect())
    }
    mlb_syntax::BasDec::Local(local_dec, in_dec) => mlb_hir::BasDec::Local(
      get_bas_dec(st, cx, *local_dec).into(),
      get_bas_dec(st, cx, *in_dec).into(),
    ),
    mlb_syntax::BasDec::Export(ns, binds) => {
      let mut names = FxHashSet::<str_util::Name>::default();
      let binds: Vec<_> = binds
        .into_iter()
        .map(|(lhs, rhs)| {
          if !names.insert(lhs.val.clone()) {
            st.errors.push(Error::new(
              ErrorSource { path: None, range: cx.group.pos_db.range_utf16(lhs.range) },
              cx.group.path.as_path().to_owned(),
              ErrorKind::Duplicate(lhs.val.clone()),
            ));
          }
          let rhs = rhs.unwrap_or_else(|| lhs.clone());
          mlb_hir::BasDec::Export(ns, lhs, rhs)
        })
        .collect();
      mlb_hir::BasDec::seq(binds)
    }
    mlb_syntax::BasDec::Path(pp) => {
      let pid = get_path_id_in_group(st.fs, st.paths, &cx.group, pp.val.as_path(), pp.range);
      let (path_id, path, source) = match pid {
        Ok(x) => x,
        Err(e) => {
          st.errors.push(e);
          cov_mark::hit("no_path");
          return mlb_hir::BasDec::seq(Vec::new());
        }
      };
      let kind = match pp.val.kind() {
        mlb_syntax::PathKind::Sml => {
          let contents = match read_file(st.fs, source, path.as_path()) {
            Ok(x) => x,
            Err(e) => {
              st.errors.push(e);
              // NOTE a bit hacky.
              String::new()
            }
          };
          st.sources.insert(path_id, contents);
          mlb_hir::PathKind::Source
        }
        mlb_syntax::PathKind::Mlb => {
          st.stack.push(GroupPathToProcess {
            parent: cx.path_id,
            range: source.range,
            path: path_id,
          });
          mlb_hir::PathKind::Group
        }
      };
      mlb_hir::BasDec::Path(path_id, kind)
    }
    mlb_syntax::BasDec::Ann(annotations, dec) => {
      let diagnostics_ignore_all = annotations.iter().any(|x| {
        let s = x.val.as_str().trim_matches('"').trim();
        s == "milletDiagnosticsIgnore all"
      });
      let inner = get_bas_dec(st, cx, *dec);
      if diagnostics_ignore_all {
        mlb_hir::BasDec::Ann(mlb_hir::Annotation::DiagnosticsIgnoreAll, inner.into())
      } else {
        inner
      }
    }
    mlb_syntax::BasDec::Seq(decs) => {
      mlb_hir::BasDec::seq(decs.into_iter().map(|dec| get_bas_dec(st, cx, dec)).collect())
    }
  }
}

fn get_bas_exp<F>(st: &mut St<'_, F>, cx: &Cx, exp: mlb_syntax::BasExp) -> mlb_hir::BasExp
where
  F: paths::FileSystem,
{
  match exp {
    mlb_syntax::BasExp::Bas(dec) => mlb_hir::BasExp::Bas(get_bas_dec(st, cx, dec)),
    mlb_syntax::BasExp::Name(name) => mlb_hir::BasExp::Name(name),
    mlb_syntax::BasExp::Let(dec, exp) => {
      mlb_hir::BasExp::Let(get_bas_dec(st, cx, dec), get_bas_exp(st, cx, *exp).into())
    }
  }
}
