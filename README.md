# malli-select

[![Clojars Project](https://img.shields.io/clojars/v/dk.thinkcreate/malli-select.svg?include_prereleases)](https://clojars.org/dk.thinkcreate/malli-select) [![cljdoc badge](https://cljdoc.org/badge/dk.thinkcreate/malli-select)](https://cljdoc.org/d/dk.thinkcreate/malli-select)

Create subschemas of [malli](https://github.com/metosin/malli)-schemas using a spec2-inspired select notation.

It's based on Rich Hickey's ideas from his talk ["Maybe Not"](https://youtu.be/YR5WdGrpoug?feature=shared&t=1965) about how [spec-alpha2](https://github.com/clojure/spec-alpha2) might allow for schema reuse.

## Quickstart

[deps-try](https://github.com/eval/deps-try/blob/master/README.md#installation) has a built-in recipe that walks you through malli-select's features on the REPL ([recipe source](https://github.com/eval/deps-try/blob/master/recipes/malli/malli_select.clj)).  
Run like so:
``` clojure
$ deps-try --recipe malli/malli-select
```

See [the tests](./test/malli_select/core_test.clj) for more.


## LICENSE

Copyright (c) 2023 Gert Goet, ThinkCreate.
Distributed under the MIT license. See [LICENSE](LICENSE).
