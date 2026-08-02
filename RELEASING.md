# Releasing

Releases are fully automated: pushing a git tag triggers the
[release workflow](.github/workflows/release.yml), which tests, builds and
deploys `dk.thinkcreate/malli-select` to [Clojars](https://clojars.org/dk.thinkcreate/malli-select).

## TL;DR

Local tags must be *signed* (`-s`):

```sh
# snapshot release (publishes X.Y.Z-SNAPSHOT)
git tag -s vX.Y.Z-pre.1 -m "vX.Y.Z-pre.1"
git push origin vX.Y.Z-pre.1

# full release (publishes X.Y.Z)
git tag -s vX.Y.Z -m "vX.Y.Z"
git push origin vX.Y.Z
```

Watch the run with `gh run watch`.

Alternatively create a [release on GitHub](https://github.com/eval/malli-select/releases/new)
(or `gh release create vX.Y.Z`) and let it create the tag — mark it as a
prerelease when using a `-pre` tag. Tags created this way aren't locally
signed but are attributed to your GitHub account.

## How it works

The workflow runs on every push to `main` and on every tag:

```sh
clojure -T:build release :build/git-version $(printf '"%s"' $(git describe --tags)) :deploy/only-jar-version-type :full-and-snapshot
```

`release` (see [build.clj](build.clj)) chains `test` → `test-cljs` → `build` → `deploy`.
The output of `git describe --tags` determines the version and whether the
built jar is actually deployed:

| `git describe --tags`             | jar version       | deployed?                     |
|-----------------------------------|-------------------|-------------------------------|
| `v1.2.3` (exact release tag)      | `1.2.3`           | yes — full release            |
| `v1.2.3-pre.1` (pre-tag, or any commit after one) | `1.2.3-SNAPSHOT` | yes — snapshot |
| `v1.2.3-5-gabc123` (commits after a release tag) | `1.2.3-5-gabc123` | no — build only |

Consequences:

- A full release requires an *exact* `vX.Y.Z` tag on the commit.
- After pushing a `vX.Y.Z-pre.N` tag, every subsequent push to `main`
  re-publishes `X.Y.Z-SNAPSHOT` — until the next exact release tag.
- Ordinary pushes to `main` after a release tag act as a dry run:
  tests run and the jar is built, but nothing is deployed.
- Pick pre-tag versions to match the *next* intended release, e.g. after
  releasing `v0.7.0` the next pre-tag should be `v0.8.0-pre.1`.
- For a full release the POM's `<scm><tag>` is set to `vX.Y.Z`.

## Credentials

`deploy` authenticates with the `CLOJARS_USERNAME` and `CLOJARS_PASSWORD`
[repository secrets](https://github.com/eval/malli-select/settings/secrets/actions).

- `CLOJARS_USERNAME`: the Clojars account name.
- `CLOJARS_PASSWORD`: a Clojars [deploy token](https://clojars.org/tokens) —
  *not* the account password. Prefer a token scoped to the `dk.thinkcreate`
  group.

To rotate: create a new token on Clojars, then
`gh secret set CLOJARS_PASSWORD --repo eval/malli-select`, and delete the old
token. The cheapest end-to-end check of the credentials is publishing a
snapshot via a `-pre` tag (see TL;DR); the token's "last used" date on the
Clojars tokens page should update.

## Local release

The same can be done locally (e.g. when CI is down):

```sh
CLOJARS_USERNAME=... CLOJARS_PASSWORD=<deploy-token> \
  clojure -T:build release :build/git-version $(printf '"%s"' $(git describe --tags)) :deploy/only-jar-version-type :full-and-snapshot
```

## Gotchas

- `git describe --tags` needs the full history: the checkout step uses
  `fetch-depth: 0` for this — keep it when touching the workflow.
- Tests run on both the JVM (`:test` alias) and ClojureScript/Node
  (`:cljs-test` alias); a failure in either suite aborts the release before
  anything is built or deployed.
