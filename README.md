# Stan

<p align="center"><img alt="Stan Logo" src="https://user-images.githubusercontent.com/8126674/83521583-59383080-a4d7-11ea-8d9e-33be4677ecb3.png" width=300px height=300px/></p>

[![GitHub CI](https://github.com/kowainik/stan/workflows/CI/badge.svg)](https://github.com/kowainik/stan/actions)
[![Hackage](https://img.shields.io/hackage/v/stan.svg)](https://hackage.haskell.org/package/stan)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/stan/blob/master/LICENSE)

Haskell static analyser

## Usage

Stan is working with the `.hie` files to analyse Haskell projects. Therefore,
Stan requires to have generated `hie` files provided by the users. Fortunately,
it is straightforward to satisfy this necessity. You can build the project with
special GHC options:

```shell
$ cabal v2-build \
  --ghc-options=-fwrite-ide-info \
  --ghc-options=-hiedir=.hie
```

Or you can also add these options to your project's `.cabal` file, so you will
always have up to date `.hie` files:

```haskell
    ghc-options:       -fwrite-ide-info
                       -hiedir=.hie
```

> _Note:_ here we recommend to generate the `hie` files into `.hie/` folder. As
> it is the recommendation only, you can specify your own folder as well. But
> then you will need to run `stan` using the `--hiedir` option with the
> specified path to your `hie` folder.
