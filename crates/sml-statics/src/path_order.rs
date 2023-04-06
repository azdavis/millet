//! Determining the order in which to analyze a set of paths that will minimize undefined
//! structure-level name errors.
//!
//! In order to conform to the CM specification, an implementation must ignore the order of source
//! files listed in the CM files, and instead determine the correct order in which to analyze the
//! files.
//!
//! How are we to do this? In order to produce a single ordering in which to visit all of the source
//! files, we must know what files define what things and what files use those things. Then we know
//! how to order the files: process first the files that define the thing, then the ones that use
//! the thing. This is a topological ordering.
//!
//! How do we know what things a file defines or uses? The CM specification guarantees that it is
//! syntactically knowable what structure level names a file defines because it bans top level open.
//! (CM also does not allow sharing non-structure-level names between files). However, knowing what
//! names a file needs from other files is less trivial.
//!
//! You may think that the answer is knowable with a similarly simple, purely syntactic analysis of
//! the file: simply traverse the syntax tree, searching for names in a "reference" position and
//! collect any structure level identifiers found.
//!
//! However, note that SML has things like `functor` and `let` and `local`, which cause structure
//! level identifiers to go in and out of scope. So we need some notion of scope. Still not too bad
//! right?
//!
//! Wrong. Things really start to get hairy when you consider `open` and `include`. Now you have the
//! ability to bring structure and signature level identifiers into scope without directly
//! syntactically mentioning them. So now we must track environments and associate them with the
//! names in scope, so we know what new names are brought into scope when we `open` a structure
//! identifier or `include` a signature.
//!
//! We also have things like functor application and signature ascription, all of which affect name
//! resolution and visibility. At this point we're starting to require quite a sophisticated level
//! of analysis just to get CM to work.
//!
//! So the approach we take is to just run all of statics on every file in a loop, but just
//! structure name resolution purposes. When a file emits no undefined errors pertaining to
//! undefined structure-level identifiers, we add this file to the ordering.
//!
//! As an "optimization", we pass to statics a mode that indicates that we don't care about anything
//! except name resolution. That means we can skip things like exhaustiveness checking and type
//! unification.

use crate::{basis::Bs, mode::Mode, st::St, sym::Syms, top_dec};

/// An unordered map from paths to HIR ready for analysis.
pub type SmlHirPaths<'a> = paths::PathMap<(&'a sml_hir::Arenas, sml_hir::StrDecIdx)>;

/// Get the ordering.
#[must_use]
pub fn get(mut syms: Syms, mut bs: Bs, mut paths: SmlHirPaths<'_>) -> Vec<paths::PathId> {
  let mut ok_paths = Vec::<paths::PathId>::new();
  for &(arenas, root) in paths.values() {
    rm_top_level_defs(&mut bs, arenas, root);
  }
  loop {
    let old_ok_paths_len = ok_paths.len();
    let mut new_paths: SmlHirPaths<'_> = fast_hash::map_with_capacity(paths.len());
    for (path, (arenas, root)) in paths {
      // NOTE: this inner loop body runs O(n^2) times. to make this hurt less, we use a special path
      // order mode for statics, which reduces the number of checks we do.
      let mut st = St::new(Mode::PathOrder, syms);
      let new_bs = top_dec::get(&mut st, &bs, arenas, root);
      let (new_syms, errors, _) = st.finish();
      syms = new_syms;
      if errors.is_empty() {
        bs.append(new_bs);
        ok_paths.push(path);
      } else {
        new_paths.insert(path, (arenas, root));
      }
    }
    paths = new_paths;
    let failed_to_advance = old_ok_paths_len == ok_paths.len();
    let nothing_left = paths.is_empty();
    if failed_to_advance || nothing_left {
      // if `failed_to_advance`, we failed to get any newly ok paths. that means there is no
      // ordering of all the paths that causes them all to have no undefined structure-level name
      // errors. so come up with a fake ordering: first the ordering we could figure out, then all
      // the other paths that we couldn't figure out, in an arbitrary but stable order.
      //
      // if `nothing_left`, we're done since we successfully processed all the paths. then the
      // ordering is exactly `ok_paths`, and `paths` is empty.
      let mut rest: Vec<_> = paths.into_keys().collect();
      rest.sort_unstable();
      ok_paths.append(&mut rest);
      return ok_paths;
    }
  }
}

fn rm_top_level_defs(bs: &mut Bs, ars: &sml_hir::Arenas, dec: sml_hir::StrDecIdx) {
  let dec = match dec {
    Some(x) => x,
    None => return,
  };
  match &ars.str_dec[dec] {
    sml_hir::StrDec::Dec(_) => {}
    sml_hir::StrDec::Structure(binds) => {
      for bind in binds {
        bs.env.str_env.remove(&bind.name);
      }
    }
    sml_hir::StrDec::Signature(binds) => {
      for bind in binds {
        bs.sig_env.remove(&bind.name);
      }
    }
    sml_hir::StrDec::Functor(binds) => {
      for bind in binds {
        bs.fun_env.remove(&bind.functor_name);
      }
    }
    sml_hir::StrDec::Local(_, in_dec) => rm_top_level_defs(bs, ars, *in_dec),
    sml_hir::StrDec::Seq(decs) => {
      for &dec in decs {
        rm_top_level_defs(bs, ars, dec);
      }
    }
  }
}
