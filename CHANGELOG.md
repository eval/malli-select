# Changelog

## Unreleased

### Added

- `prune-optionals`: standalone pruning of any schema — removes every optional subtree
  that contains no required attribute. Takes an optional keep-selection
  (positional, selection notation): mentioned entries are kept but stay
  optional. For `:multi` schemas, mentioning branches keeps only those
  branches. See the README's "Pruning" section.
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
- `selectable-paths` and `selector` take an optional `options` argument.

### Fixed

- `options` of `select`, `select-only`, `prune-optionals`, `selector` and
  `selectable-paths` are now passed to `malli.core/schema`. So malli options
  like `:registry` work when you pass a schema as a vector:
  `(select [:map [:name ::my-type]] [:name] {:registry registry})`. Before
  this change it threw `:malli.core/invalid-schema`.
- The form of a selected schema is now faithful for registry references.
  Before, a reference kept its name (`::address`, `[:ref ::address]`) while
  selection changed its target, so `(m/schema (m/form selected) opts)` gave
  back the original, unselected schema. Now a reference whose target changed
  is inlined; an unchanged reference keeps its name. **Visible change:** forms
  of selections on schemas with references look different (inlined maps
  instead of names).
- Selecting from a schema with an explicit `[:ref ...]` threw
  `:malli.core/invalid-ref`. It works now, and `selectable-paths` lists paths
  through `[:ref ...]`.
- Recursive schemas now throw `::recursive-ref-unsupported` (with the ref in
  `ex-data`) instead of `:malli.core/invalid-ref`. Recursion isn't supported
  yet: an inlined selection would never end.
- Selection no longer walks into `:not` and function schemas (`:=>`, `:->`,
  `:function`); they're left untouched. Before, maps inside them were
  optionalized. Under `:not` that flipped the meaning: e.g. the guard
  `[:or [:not [:map [:a :any] [:b :any]]] [:fn ...]]` stopped working, so the
  `:fn` always ran. For function schemas it changed the contract (optional
  argument keys). `selectable-paths` no longer lists paths below these schemas.
  A selection into them, e.g. `[{:f [:id]}]` for `[:f [:=> ...]]`, now throws
  `::unknown-paths`.

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
