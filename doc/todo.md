# TODO

priority is approximate. if unsure of priority, put in low.

## high

- get logo
  - stick in readme
  - and 'icon' field of package.json
  - that icon needs to be 128x128 png
- impl modules
- impl more LSP features
  - jump to definition
  - hover for type/documentation/info
- add more of the standard basis
  - https://smlfamily.github.io/Basis/
  - bootstrap?
- make it performant
  - only lower the parts of the syntax tree that changed?
  - only re-check those parts?
  - don't re-calculate everything every time LOL
  - salsa-rs?

## medium

- improve statics for type variables
  - implicit scoping
  - performing closure operations
  - checking whether expressions are expansive (oh no!)
  - type variables in signatures
  - handle equality types
  - note ignored tests
- get better error messages
  - show witnesses to non-exhaustive match
  - improve ranges of stuff
  - show fully qualified names in type errors
  - related: avoid clashes between ty vars
  - show something better than `_` for meta vars?
  - "special case" some constructs instead of just blindly `unify`ing?
    - in such a way as to not allow/reject more programs, just improve errors
  - better overload errors: maybe we should choose what that overloaded type should be sooner
  - better parser errors?
- support files exporting/importing from each other
  - via CM? (guh)
  - via e.g. a `project.json` (or `.txt`? or `.toml`?) in the workspace root which would list the ordered files in this project
  - just require files to `use` each other?

## low

- make website
  - with 'verified ownership' or whatever
- parse more derived forms
  - functor binding input sugar
    - or skip this? seems to be confusing to students
  - specifications
  - signature expressions
  - programs
- implement statics for weird constructs
  - `abstype`
  - `#` selectors
  - more generally, `...` pats
  - `sharing` spec
- impl more tools
  - auto formatter
  - style linter
    - unused variable
    - use of `;`, `while`, selectors
    - unnecessary `()`
    - `if x then false else true`, etc
  - interpreter (hard)
- try not to clone the whole damn cx in statics?
  - especially in dec?
- rm 'old-todo'
- use `type Seq<T> = Box<[T]>` instead of `Vec<T>` in hir, etc?
- forbid symbolic names in some cases in parsing?
- talk about how statics errors should use an abstract 'Entity' (currently just 'this expression' or 'this declaration' etc) and this entity gets turned into an actual text range somewhere else (lower::Ptrs)
- could add more to this, like 'the name of the third con bind in the second dat bind of this datatype dec'
