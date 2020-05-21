#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SourceFileId(usize);

impl SourceFileId {
  pub fn new(id: usize) -> Self {
    Self(id)
  }
}

pub struct SourceFile {
  name: String,
  bytes: Vec<u8>,
  id: SourceFileId,
}

impl SourceFile {
  pub fn new(name: String, bytes: Vec<u8>, id: SourceFileId) -> Self {
    Self { name, bytes, id }
  }

  pub fn as_bytes(&self) -> &[u8] {
    &self.bytes
  }

  pub fn id(&self) -> SourceFileId {
    self.id
  }
}
