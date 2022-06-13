use once_cell::sync::Lazy;

const ORDER: &[&str] = &[
  include_str!("list-pair.sml"),
  include_str!("list.sml"),
  include_str!("option.sml"),
];

pub(crate) static SYMS_AND_BS: Lazy<(statics::Syms, statics::Bs)> = Lazy::new(|| {
  let mut st = statics::Statics::default();
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
    statics::get(&mut st, mode, &lowered.arenas, &lowered.top_decs);
    if let Some(e) = st.errors.first() {
      panic!("std_basis error: {}", e.display(&st.syms));
    }
  }
  (st.syms, st.bs)
});
