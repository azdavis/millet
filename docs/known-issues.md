# Known issues

- The distinction between equality types and non-equality types is ignored.
- Some features like `abstype` are not supported.
- Paths with certain 'default' path variables are ignored. (This is because Millet includes the std basis and other definitions.)
- CM support is incomplete.
  - The preprocessor is ignored.
  - Tool options are not supported.
  - String paths (wrapped in quotes) are not supported.
- ML Basis support is incomplete.
  - Annotations are ignored.
  - String paths (wrapped in quotes) are not supported.
- The standard basis is made available to all files, regardless of whether files ask for it.
- Every file is entirely re-analyzed upon a single file change. This can make the server slow.
