# TODO

priority is approximate. if unsure of priority, put in low.

## high

- get logo
  - stick in readme
  - and 'icon' field of package.json
  - that icon needs to be 128x128 png
- support multiple files better
  - already have rudimentary CM
  - maybe use MLB instead
- support no root
  - just analyze each file in isolation
- impl more LSP features
  - jump to definition
  - hover for type/documentation/info

## medium

- improve statics for type variables
  - performing closure operations
  - checking whether expressions are expansive (oh no!)
- handle equality types
  - in type variables
  - `eqtype` vs `type`
  - various side conditions on modules, `datatype`, possibly others
- get better error messages
  - improve ranges of stuff (names?)
  - show fully qualified names in type errors
  - related: avoid clashes between ty vars
  - show something better than `_` for meta vars?
  - better parser errors?
- make it performant
  - only lower the parts of the syntax tree that changed?
  - only re-check those parts?
  - don't re-calculate everything every time LOL
  - salsa-rs?

## low

- make website
  - with 'verified ownership' or whatever
- implement statics for weird constructs
  - `abstype`
  - `#` selectors
  - more generally, `...` pats
- impl more tools
  - auto formatter
  - style linter
    - unused variable
    - use of `;`, `while`, selectors
    - unnecessary `()`
    - `if x then false else true`, etc
  - interpreter (hard)
- use `type Seq<T> = Box<[T]>` instead of `Vec<T>` in hir, etc?
- forbid symbolic names in some cases in parsing?
