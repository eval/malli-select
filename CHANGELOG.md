# Changelog

## Unreleased

### Added

- `select-only`: explicit alternative to `^:only` metadata /
  `{:prune-optionals true}`; forces pruning (also when the caller passes
  `:prune-optionals false`).
- `:multi` schema support: branches are selected with a *map*-selection —
  keys are dispatch values, values are sub-selections. `{:human [:name]}`
  requires `:name` in branch `:human`; unmentioned branches become
  all-optional. `'*` as branch key addresses every branch (its sub-selection
  must be satisfiable in each; `'*` merges with explicit branch keys). A
  keyword `:dispatch` key is auto-required in every explicit map-branch.
  Pruning (`^:only`) drops unmentioned branches (mentioning none keeps all).
  `selectable-paths` and verification errors spell branch segments as
  `{:branch dispatch-value}`, and shape mismatches (vector selection on a
  `:multi`, map selection on a `:map`) come with a `:hint`.
  See the README's "Multi-schemas" section, incl. limitations.

- ClojureScript support: the library is now `.cljc` and tested on Node
  (`clojure -M:cljs-test`) as well as the JVM.
- `:verify-selection` accepts two new values besides `:throw`/`:assert` and
  the `:skip`/`nil`/`false` opt-outs: `:log` (print a warning — stderr on
  Clojure, `console.warn` on ClojureScript — and continue) and a function
  (called with `{:paths ... :available ...}`, selection continues).
- The dynamic var `*verify-selection*` (initially `:throw`) provides the
  default for the `:verify-selection` option; an explicitly passed option
  still wins.

### Breaking

- An invalid selection now throws an `ExceptionInfo` instead of an `AssertionError`.
  This makes it catchable via `(catch ExceptionInfo e ...)` and works in ClojureScript
  (where asserts may be elided in release builds). The unknown and available paths are
  available as data:
  ```clojure
  (ex-data e)
  ;; => {:type :malli-select.core/unknown-paths
  ;;     :data {:paths (...), :available (...)}}
  ```
  If you were catching `AssertionError` or matching the assert message, update your code.
  The preferred spelling of the `:verify-selection` option is now `:throw` (the default);
  `:assert` still works, as do the `:skip`/`nil`/`false` opt-outs.
- Selections with duplicate keys now merge instead of "last wins"
  ([`404d677`](https://github.com/eval/malli-select/commit/404d677)).
  `[{:friends [:name]} {:friends [:age]}]` now requires both `:name` and
  `:age` of friends; it used to require only `:age`. The merge is a union of
  required paths, so results only get stricter, and it applies at every
  nesting level.
  Consequences:
  - To override instead of merge, build a single map yourself.
  - A later `{:friends []}` is now a no-op; it used to reset the
    `:friends` sub-selection to all-optional.
  - With `:prune-optionals` / `^:only`, merged selections keep more
    attributes, so generated samples can gain fields.

Older releases are documented on the
[GitHub releases page](https://github.com/eval/malli-select/releases).
