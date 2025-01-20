# Changelog

`stan` uses [PVP Versioning][1].
The change log is available [on GitHub][2].

## 0.2.0.0

* Add the following inspections:

    * Partial instance for 'Scientific' method 'GHC.Real.fromRational' (`stan0022`)
    * Partial 'Scientific' function 'GHC.Real.realToFrac' (`stan0023`)
    * Partial instance for 'Scientific' method 'GHC.Real.recip' (`stan0024`)
    * Partial instance for 'Scientific' method 'GHC.Real.(/)' (`stan0025`)

  Since these change the behaviour of `stan` by default this is a
  major release.

* Support GHC 9.12 (thanks to @ncaq)

## 0.1.3.0

* Add prospective support for GHC 9.10

  * thanks to @philderbeast

## 0.1.2.1

* Support `clay-0.15` series.

* Support `base64-1.0` series.

## 0.1.2.0

* Added `runStan`, `getAnalysis`, `getStanConfig`

## 0.1.1.0

* Fix [bug #541](https://github.com/kowainik/stan/issues/541)
  "`nodeInfo`"

## 0.1.0.2

* Add prospective support for GHC 9.8

  * will only work with `Cabal` library version 3.10 -- if this causes
    problems for you please comment on the corresponding [`extensions`
    ticket](https://github.com/kowainik/extensions/issues/89)

  * thanks to @0rphee

  * we don't provide a binary release for 9.8.1 because `stan`'s
    dependencies have not yet caught up with 9.8.1.  If you want to
    use `stan` with 9.8.1 you can install it from Hackage with `cabal
    install stan --allow-newer`.

## 0.1.0.1

* Add support for GHC 9.6 (will only work with `Cabal` library version
  3.10 -- if this causes problems for you please comment on the
  corresponding [`extensions`
  ticket](https://github.com/kowainik/extensions/issues/89))

## 0.1.0.0

* Add support for GHCs 9.0, 9.2 and 9.4
* [#55](https://github.com/kowainik/stan/issues/55):
  Implement single-pass HIE AST traversal.
* [#348](https://github.com/kowainik/stan/issues/348):
  Compress binaries for GitHub releases.
* [#368](https://github.com/kowainik/stan/issues/368):
  Fix inspections for `unordered-containers` functions to support the
  latest package version.

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
