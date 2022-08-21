# Known issues

See also the [TODO][].

[CMU 15-150][15-150] students might want to check out [this doc][for-150].

- The distinction between equality types and non-equality types is ignored.
- Some language constructs like `abstype` are not supported.
- Paths with certain 'default' path variables are ignored. (This is because Millet includes the std basis and other definitions.)
- CM support is rudimentary.
  - Files are analyzed in the order listed in the CM files.
  - The preprocessor is ignored.
  - String paths (wrapped in quotes) are not supported.
- ML Basis support is limited.
  - Annotations are ignored.
  - String paths (wrapped in quotes) are not supported.
- The standard basis is made available to all files, regardless of whether files ask for it.
- Every file is entirely re-analyzed upon a single file change. This can make the server slow.

[todo]: /docs/todo.md
[for-150]: /docs/for-15-150.md
[15-150]: https://www.cs.cmu.edu/~15150/
