# TODO

- improve statics for type variables
  - implicit scoping
  - performing closure operations
  - checking whether expressions are expansive (oh no!)
  - type variables in signatures
- fix statics for equality types
  - some TODOs are in the code
- fix statics for functors
  - failing skipped tests are in tests/
- parse derived forms
  - functor binding input sugar
    - or skip this? seems to be confusing to students
  - specifications
  - signature expressions
  - programs
- support many files
  - via CM? (guh)
  - via `millet.json` in the workspace root which would list the ordered files
    in this project
- implement statics for unused constructs (abstype, while, `#` selectors)?
- make public
  - turn on protected branches, etc
  - use github actions ci
- publish extension
  - get azure account or whatever
- get better error messages
  - improve locs for signature matching
  - prefer 'expected int list, found bool list' instead of 'expected int, found
    bool' and similar?
  - show fully qualified names in type errors
  - better overload errors: maybe we should choose what that overloaded type
    should be sooner
  - better parser errors?
- impl more LSP features
  - jump to definition
  - hover for type/documentation/info
- impl more tools
  - auto formatter
  - style linter
  - interpreter (hard)
