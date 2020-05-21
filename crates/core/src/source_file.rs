pub struct SourceFile {
  name: String,
  bytes: Vec<u8>,
}

impl SourceFile {
  pub fn new(name: String, bytes: Vec<u8>) -> Self {
    Self { name, bytes }
  }

  pub fn as_bytes(&self) -> &[u8] {
    &self.bytes
  }
}
