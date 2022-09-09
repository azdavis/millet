# TODO

priority is approximate. if unsure of priority, put in low.

## medium

- handle equality types
  - in type variables
  - `eqtype` vs `type`
  - various side conditions on modules, `datatype`, possibly others
- improve hover for type
  - hover on structure/signature/functor?
- add assists
  - fill struct ascribing to signature with "empty" stuff
  - rewrite constructs that wouldn't pass style check to ones that would
- get better error messages
  - improve ranges of stuff (names?)
  - show fully qualified names in type errors
  - related: avoid clashes between ty vars
- make it more performant
  - don't re-IO, re-lex, re-parse, re-lower every file every time
  - only lower the parts of the syntax tree that changed?
  - only re-statics-check those parts?
  - salsa-rs?

## low

- implement statics for `abstype`
- impl more tools
  - auto formatter
  - style linter
    - unused variable
    - use of `;`, `while`, selectors
    - unnecessary `()`
    - `if x then false else true`, etc
  - interpreter (hard)
- forbid symbolic names in some cases in parsing?
