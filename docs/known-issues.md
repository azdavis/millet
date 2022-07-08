# Known issues

See also the [TODO][].

[CMU 15-150][15-150] students might want to check out [this doc][for-150].

- No error is emitted for the value restriction.
- The distinction between equality types and non-equality types is ignored.
- Some language constructs like `abstype` are not supported.
- CM support is rudimentary.
  - Files are analyzed in the order listed in the CM files.
  - The preprocessor is ignored.
  - String paths (wrapped in quotes) in CM files are not supported.
  - CM files are not analyzed to limit exports.
  - `$` paths are ignored.
  - The standard basis is made available to all files, regardless of whether files ask for it via CM or not.
- ML Basis support is rudimentary.
  - Everything except lists of files are ignored. This means `local`, `basis`, export renames, annotations, etc are all ignored.
  - `$` paths are ignored.
- Every file is entirely re-analyzed upon a single file change. This can make the server slow.

[todo]: /docs/todo.md
[for-150]: /docs/for-15-150.md
[15-150]: https://www.cs.cmu.edu/~15150/
