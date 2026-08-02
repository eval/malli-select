# Changelog

## Unreleased

### Added

- ClojureScript support: the library is now `.cljc` and tested on Node
  (`clojure -M:cljs-test`) as well as the JVM.

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

[Unreleased]: https://github.com/eval/malli-select/compare/v0.7.0...HEAD
