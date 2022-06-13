const ORDER: &[&str] = &[
  include_str!("list-pair.sml"),
  include_str!("list.sml"),
  include_str!("option.sml"),
];

pub(crate) fn get_statics() -> statics::Statics {
  let mut ret = statics::Statics::default();
  let mode = statics::Mode::Declaration;
  for &contents in ORDER {
    let lexed = lex::get(contents);
    if let Some(e) = lexed.errors.first() {
      panic!("std_basis error: {}", e.kind);
    }
    let parsed = parse::get(&lexed.tokens);
    if let Some(e) = parsed.errors.first() {
      panic!("std_basis error: {}", e.kind);
    }
    let mut lowered = lower::get(&parsed.root);
    if let Some(e) = lowered.errors.first() {
      panic!("std_basis error: {}", e.kind);
    }
    ty_var_scope::get(&mut lowered.arenas, &lowered.top_decs);
    statics::get(&mut ret, mode, &lowered.arenas, &lowered.top_decs);
    if let Some(e) = ret.errors.first() {
      panic!("std_basis error: {}", e.display(&ret.syms));
    }
  }
  ret
}
