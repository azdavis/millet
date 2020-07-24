# TODO

- statics for type variables
  - implicit scoping
  - performing closure operations
  - checking whether expressions are expansive (oh no!)
- better statics for equality types
- statics for functors
- parse derived forms
  - functor binding input sugar
  - specifications
  - signature expressions
  - programs
- statics for unused constructs (abstype, while, selectors)?
- make public
  - turn on protected branches, etc
  - use github actions ci
- publish extension
  - get azure account or whatever
- better error messages
  - improve locs for signature matching
  - prefer 'expected int list, found bool list' instead of 'expected int, found
    bool' and similar?
  - show fully qualified names in type errors
  - better overload errors: maybe we should choose what that overloaded type
    should be sooner
  - better parser errors?
- handle CM? (guh)
- more LSP features
  - jump to def
  - hover for type
  - many files, maybe via `millet.json` in the workspace root
