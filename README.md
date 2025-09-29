# Plu-Stan

[![GitHub CI](https://github.com/input-output-hk/plu-stan/actions)](https://github.com/input-output-hk/plu-stan/actions)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/stan/blob/main/LICENSE)

Plu-Stan is a [Plinth](https://github.com/input-output-hk/plutus) **ST**atic **AN**alysis tool based on the [Stan](https://github.com/kowainik/stan) static analysis tool.

> ⚠️ Note: Plu-Stan is currently a Proof of Concept (PoC) and is not yet ready for production use. It is being actively developed and improved.


## Table of Contents

- [Plu-Stan](#plu-stan)
  - [Table of Contents](#table-of-contents)
  - [What this tool is about](#what-this-tool-is-about)
  - [Rules](#rules)
  - [Usage](#usage)
    - [Building & Running](#building--running)
    - [Running tests](#running-tests)
    - [Haskell language server integration](#haskell-language-server-integration)
## What this tool is about

[[Back to the Table of Contents] ↑](#table-of-contents)

Plu-Stan is a static analysis tool for Cardano Smart Contracts written in Plinth. It is based on the [Stan](https://github.com/kowainik/stan) static analysis tool.
The goal of the project is to help Plinth developers to write better code by providing meaningful insights and suggestions on how to improve it, both in terms of code security and performance.

Plu-Stan design and implementation is based on Stan. On top of that, Plu-Stan has its own goals and objectives that are:

- Catch common security issues in Plinth code
- Suggest performance improvements specific to Plinth
- Provide meaningful recommendations and solutions to the problems found
- Help beginners to learn best practices in an easy and informative way

## Rules

[[Back to the Table of Contents] ↑](#table-of-contents)

On top of all the rules provided by Stan, Plu-Stan implements its own set of rules specific to Plinth. The rules are divided into the following categories:
- **Security**: rules that catch common security issues in Plinth code.
- **Performance**: rules that suggest performance improvements specific to Plinth.

So far, Plu-Stan implements the following rules:
| ID          | Name                                                 | Category    | Severity  |
|-------------|------------------------------------------------------|-------------|-----------|
| PLU-STAN-01 | Usage of 'unsafeFromList' can lead to runtime errors | Security    | High      |
| PLU-STAN-02 | Usage of 'unsafeFromBuiltinData' can lead to unexpected behavior | Security    | High      |
| PLU-STAN-03 | Usage of Optional types in on chain code. |           | Security    | Medium    |
| PLU-STAN-04 | Usage of 'eq instance' on script hash, public key hash and payment credential | Security    | Medium    |

## Usage
[[Back to the Table of Contents] ↑](#table-of-contents)
  
### Building & Running

NOTE: Because of cardano specific libraries, we need the following external packages installed: `secp256k1`, `sodium` and `blst`. Follow the instructions from the official `cardano-node` install [page](https://developers.cardano.org/docs/operate-a-stake-pool/node-operations/installing-cardano-node/).

We used GHC version 9.6.6 for development. Other versions supported by Plutus and Stan should work as well. Please report any issues.


  1. To build the project, run:
     ```bash
     cabal build
     ```
  2. To run the tool, inside this project directory, use:
     ```bash
     cabal run stan
     ```

  3. To run it outside this project directory, use:
     ```bash
     cabal list-bin exe:stan
     # and use the path to the binary where you need it

     # or to install it globally
     cabal install exe:stan
     ```
### Running tests

To run the tests, use:
```bash
cabal test
# or to see the full output
cabal run stan-test
  ```

### Haskell language server integration

There is a fork of haskell language server that uses plu-stan as the stan plugin alternative.
In order to use that version, simply clone [Input-output's HLS](https://github.com/input-output-hk/haskell-language-server) repository, and follow the instructions:

```bash
git checkout feat/plu-stan
cabal update
cabal build

cabal list-bin exe:haskell-language-server
# copy the path to the binary and use it in your IDE settings
```

These are two examples of configuration for VSCode and Neovim:

- VSCode:
```json
{
    "haskell.serverExecutablePath": "/path/to/your/haskell-language-server-wrapper",
    "haskell.plugin.stan.globalOn": true
}
```
- Neovim (using nvim-lspconfig):
```lua
{
    cmd = { "/path/to/your/haskell-language-server-wrapper", "--lsp" },
    settings = {
        haskell = {
            ... other settings ...
            plugin = {
                stan = { globalOn = true },
            },
        },
    },
}
```
