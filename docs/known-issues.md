# Known issues

See also the [TODO][].

[CMU 15-150][15-150] students might want to check out [this doc][for-150].

- The distinction between equality types and non-equality types is ignored.
- Some language constructs like `abstype` are not supported.
- Default path variables are not defined.
- CM support is rudimentary.
  - Files are analyzed in the order listed in the CM files.
  - The preprocessor is ignored.
  - String paths (wrapped in quotes) in CM files are not supported.
  - CM files are not analyzed to limit exports.
  - The standard basis is made available to all files, regardless of whether files ask for it via CM or not.
- ML Basis support is limited.
  - Annotations are ignored.
- Every file is entirely re-analyzed upon a single file change. This can make the server slow.

[todo]: /docs/todo.md
[for-150]: /docs/for-15-150.md
[15-150]: https://www.cs.cmu.edu/~15150/
