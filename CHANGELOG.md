# Changelog

`stan` uses [PVP Versioning][1].
The change log is available [on GitHub][2].

## Unreleased

* [#55](https://github.com/kowainik/stan/issues/55):
  Implement single-pass HIE AST traversal.

## 0.0.1.0 — Jul 9, 2020

* [#320](https://github.com/kowainik/stan/issues/320):
  Add `-b|--browse` option to the `report` command.
* [#327](https://github.com/kowainik/stan/issues/327):
  When the generated HIE files are incomplete (missing the source code),
  print `<UNAVAILABLE>` as the source instead of failing.
* [#329](https://github.com/kowainik/stan/issues/329):
  Add GHC version to the `--version` output.
* [#326](https://github.com/kowainik/stan/issues/326):
  Handle constraints before constructors in `STAN-0206`.
* [#323](https://github.com/kowainik/stan/issues/323):
  Add `--json-output` option that output the results in machine readable JSON
  format instead. Also all other printing is turned off then.
* Minor documentation improvements.

## 0.0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/stan/releases
