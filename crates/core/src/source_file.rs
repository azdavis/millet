pub struct SourceFile {
  pub name: String,
  pub bytes: Vec<u8>,
}

impl SourceFile {
  pub fn as_bytes(&self) -> &[u8] {
    &self.bytes
  }
}
