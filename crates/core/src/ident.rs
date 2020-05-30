#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ident {
  inner: String,
}

impl Ident {
  pub fn new(inner: String) -> Self {
    Self { inner }
  }
}
