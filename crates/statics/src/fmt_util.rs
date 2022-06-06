/// returns a char iterator that when collected could be a name for a type variable.
pub(crate) fn ty_var_name(equality: bool, idx: usize) -> impl Iterator<Item = char> {
  let alpha = (b'z' - b'a') as usize;
  let quot = idx / alpha;
  let rem = idx % alpha;
  let ch = char::from((rem as u8) + b'a');
  let ticks = if equality { 1 } else { 2 };
  std::iter::repeat('\'')
    .take(ticks)
    .chain(std::iter::repeat(ch).take(quot))
}
