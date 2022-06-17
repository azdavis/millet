# Known issues

See also the [TODO][].

- No error is emitted for the value restriction.
- The distinction between equality types and non-equality types is ignored.
- Some language constructs like `abstype` and `...` pattern rows are not supported.
- CM files are not analyzed to limit exports.
- The standard basis is made available to all files, regardless of whether files ask for it via CM or not.
- The full CM spec (preprocessor, the "set calculus" for exports) is not supported.
- MLBasis files are not supported.
- Every file is entirely re-analyzed upon a single file change. This can make the server slow.

[todo]: /doc/todo.md
