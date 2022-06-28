# TODO

priority is approximate. if unsure of priority, put in low.

## high

- get art
  - stick image in readme
  - add 'icon' field of package.json (128x128 png)
- support multiple files better
  - improve CM, currently rudimentary
  - and/or impl MLB support instead

## medium

- improve statics for type variables
  - performing closure operations
  - checking whether expressions are expansive (oh no!)
- handle equality types
  - in type variables
  - `eqtype` vs `type`
  - various side conditions on modules, `datatype`, possibly others
- improve hover
  - hover on structure/signature/functor?
- improve docs
  - add more
  - use md features
  - allow in non-std-basis?
- add assists
  - fill case
  - rewrite constructs that wouldn't pass style check to ones that would
- get better error messages
  - improve ranges of stuff (names?)
  - show fully qualified names in type errors
  - related: avoid clashes between ty vars
  - better parser errors?
- make it more performant
  - don't re-IO, re-lex, re-parse, re-lower every file every time
  - only lower the parts of the syntax tree that changed?
  - only re-statics-check those parts?
  - salsa-rs?

## low

- make website
  - with 'verified ownership' or whatever
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
