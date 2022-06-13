const ORDER: [&str; 0] = [];

pub(crate) fn get_statics() -> statics::Statics {
  let mut ret = statics::Statics::default();
  let mode = statics::Mode::Declaration;
  for contents in ORDER {
    let lexed = lex::get(contents);
    assert!(lexed.errors.is_empty());
    let parsed = parse::get(&lexed.tokens);
    assert!(parsed.errors.is_empty());
    let mut lowered = lower::get(&parsed.root);
    assert!(lowered.errors.is_empty());
    ty_var_scope::get(&mut lowered.arenas, &lowered.top_decs);
    statics::get(&mut ret, mode, &lowered.arenas, &lowered.top_decs);
    assert!(ret.errors.is_empty());
  }
  ret
}
