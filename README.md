# Stan

<p align="center"><img alt="Stan Logo" src="https://user-images.githubusercontent.com/8126674/83521583-59383080-a4d7-11ea-8d9e-33be4677ecb3.png" width=300px height=300px/></p>

[![GitHub CI](https://github.com/kowainik/stan/workflows/CI/badge.svg)](https://github.com/kowainik/stan/actions)
[![Hackage](https://img.shields.io/hackage/v/stan.svg)](https://hackage.haskell.org/package/stan)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/stan/blob/master/LICENSE)

Stan is a Haskell **ST**atic **AN**alysis tool.

> ⚠️ Note: Stan is in the beta phase. The API is the subject to be
> changed if required by our needs ⚠️

## What this tool about

Stan is a command-line tool for analysing Haskell projects and
outputting discovered vulnerabilities in a helpful way with possible
solutions for detected problems. Stan is searching for not only
performance or error-prone code pieces, but it also can help with
establishing and applying best-practices from the whole Haskell
ecosystem.

Although Haskell is a statically typed language, not all properties
can be encoded in types. Even though GHC is quite a powerful compiler,
it tries to be library-agnostic and provide only language-specific
suggestions, while Stan uses the knowledge about the current state of
the ecosystem and commonly used libraries.

You will find Stan helpful if you enjoy writing in Haskell, but want
more guarantees from your code, not provided by the Haskell type
system or GHC.

## Goals

Stan design and implementation is driven by the following goals:

- Catch common vulnerabilities, anti-patterns, performance issues
- Provide meaningful insights on the projects generally
- Point out potential bugs and weak points in the programs flow for
  users, so they can carefully evaluate each problem with the code
- Help beginners to learn best practices in an easy and informative way
- Generate the report that can be used as a proof of code quality
- Create best in the class and flexible enough interface for usage
  (including e.g. opt-in and opt-out inspections)

## Features

Stan is a configurable CLI tool. Besides the main feature of analysing
Haskell projects statically, Stan has a list of features that make it
unique, easy to use and flexible to configure:

- Pretty analysis results, including both HTML and terminal reports
- Suggestions and possible solutions for fixing the existing problems
- Analysing not only Haskell source code, but also information from
  the `.cabal` files
- Flexible runtime configuration via [TOML][toml] and CLI

You can see an example of Stan HTML report hosted online here:

* [Stan Report Example](https://kowainik.github.io/projects/stan/report)

The below example of the terminal output gives you the understanding
of what sorts of analysis you can expect from Stan:

![Stan terminal example](https://user-images.githubusercontent.com/4276606/85208720-89326100-b32a-11ea-94fd-4aa149626b74.png)

## How it works

Stan analysis is based on the [HIE files][hie] — compile-time
information about Haskell source code gathered and recorded by
GHC. The HIE files contain the Haskell AST, detailed information about
each identifier and types of all expressions and sub-expressions. GHC
does a huge amount of work when compiling the Haskell projects, and
Stan takes advantage of this feature to avoid duplicating the work and
focus more on the unique features.

To analyse HIE files easily, we developed an eDSL for defining AST and
Type patterns based on the
[_final taggless_](http://okmij.org/ftp/tagless-final/course/lecture.pdf)
approach. Stan algorithm traverses HIE AST for each HIE file in the
project, and matches every AST node with the given pattern to find
potential vulnerabilities in the code.

Each Stan analysis check is represented by the __inspection__ with the
unique ID. Each inspection has a name, description, __severity__, list
of __categories__, pattern for matching relevant parts of source code
and possible solutions to the problem.

When an inspection is casted on the project, it produces zero or more
__observations__ — vulnerabilities in the specific parts of the
code. You can think of an _observation_ as a pair of an inspection and
a piece of source code where this inspection was triggered. Each
observation is assigned an unique stable ID depending on the source
location, so you can refer to them later or ignore.

You can disable inspections or enable them only in particular modules
using __check__ — rules for controlling which inspections to run and
where. Each check has a __type__ (`include` or `exclude`), __filter__
(by inspection id, category, severity, etc.) and __scope__ (file,
directory, everything). Checks can be specified using either TOML of
CLI interfaces. By default, Stan analyses all source files using all
implemented inspections.

If you want to understand Stan terminology better, refer to the
glossary:

 * [Stan Wiki page: Glossary][glossary]

## Installation instructions

Stan takes advantage of the
[GHC API](http://hackage.haskell.org/package/ghc) to provide its
analysis. Because of this, Stan and the analysed project need to be
built with the same GHC version (for more details see
[#178](https://github.com/kowainik/stan/issues/178)). That is why the
easiest and most robust way to install Stan is to build it from
sources on your machine.

> __Note:__ Stan is compatible with the GHC versions ⩾ 8.8

### Using Cabal

Below are the steps to install Stan using the Cabal build tool.

> You need to have [Cabal ⩾ 2.4](https://www.haskell.org/cabal/)

First, you need to clone the repository:

```shell
$ git clone https://github.com/kowainik/stan.git
$ cd stan
```

Then, you need to build it using Cabal:

```shell
$ cabal v2-build exe:stan
```

Finally, you can copy the resulting executable under the desired
location (that should be under the PATH environment variable), like
so:

```shell
$ cp dist-newstyle/build/x86_64-linux/ghc-8.8.3/stan-0.0.0.0/x/stan/build/stan/stan ~/.local/bin/stan
```

> The path to the executable will be outputted as the last line of the
> previous command.

### Using Stack

Below are the steps to install Stan using the Stack build tool.

> You need to have [Stack ⩾ 2.1.3](http://haskellstack.org)

First, you need to clone the repository.

```shell
$ git clone https://github.com/kowainik/stan.git
$ cd stan
```

Then, you need to build it using Stack:

```shell
$ stack build
```

Finally, you can copy the resulting executable under the desired
location (that should be under the PATH environment variable), like
so:

```shell
$ cp "$(stack path --local-install-root)/bin/stan" ~/.local/bin/stan
```

## Usage instructions

Stan works with the [HIE files][hie] to analyse Haskell
projects. Therefore, Stan requires users to generate HIE files in
advance. Fortunately, it is straightforward to satisfy this
necessity. To produce HIE files, add the following GHC options in your
project's `.cabal` file to each stanza you want to analyse:

```haskell
    ghc-options:       -fwrite-ide-info
                       -hiedir=.hie
```

> _Recommendation_: you can use the [common
> stanzas](https://vrom911.github.io/blog/common-stanzas) feature to
> write the above options only once and enable them in each stanza
> easily.

> _Note:_ here we recommend generating the HIE files into `.hie/`
> folder. As it is the recommendation only, you can specify your own
> folder as well. But then you will need to run `stan` using the
> `--hiedir` option with the specified path to your `hie` folder.

After creating HIE files, you can just run Stan on the project:

```shell
$ stan
```

to see all found vulnerabilities in your terminal.

If you want to see a more detailed information in a more structured
way, you can generate an HTML report (to the `stan.html` file) using
the following command:

```shell
$ stan report
```

Stan strives to implement the convenient interface, so you can use the
tool without configuring a lot in advance. However, the tool also
provides various ways to set it up in the way to be the most efficient
with your particular use case.

### General configuration info

Stan's work can be configured from the multiple sources (in increasing
order of priority):

1. Default settings (hard-coded in the library — includes __no__
   custom settings)
2. Environment variables
3. [TOML][toml] file configuration
4. CLI arguments

Stan runtime settings have many parts, and each of them can come from
different configuration sources. If some option is specified through
multiple sources, the most prioritized one will be used. In addition,
Stan helps to understand its own configuration, so it outputs detailed
information about each part of the config, what configuration settings
were used and how they were set.

![Configuration explanation](https://user-images.githubusercontent.com/4276606/85208889-bb908e00-b32b-11ea-8256-c576a33cbf38.png)

### TOML configurations

Stan supports [TOML][toml] runtime configuration in order to customize
the work of the tool based on the user's individual requirements. You
can use the TOML configuration to disable some inspections, enable
them only in particular Haskell modules, ignore some observations or
completely remove some files from the analysis.

See Haddock documentation for explanation of how the TOML
configuration works and examples of the different use cases.

In case you have a number of TOML files locally, the following rules
describe how Stan decides which TOML configuration file to use:

* By default, Stan tries to read settings from the local `.stan.toml`
  file in the current directory. So, if you want to adjust the default
  Stan settings with some custom rules, create a `.stan.toml` file in
  the root of your Haskell project.
* If the local `.stan.toml` file is not found, Stan tries to read the
  global `~/.stan.toml` file. Having a global Stan configuration can
  be convenient, if you work on several projects and want to have the
  same custom settings by default for all of them.
* If you don't have any of the default configuration files, it is
  still okay. Stan will use its own default hard-coded settings.
* You can specify a path to a specific configuration file using the
  `--config-file` option. This custom file will be used in addition to
  the default TOML config.
* If you don't want to use the default TOML configuration, pass the
  `--no-default` flag or use the `STAN_USE_DEFAULT_CONFIG=False`
  environment variable.

### Command-line Interface

This section describes what is possible to achieve with the Stan
CLI. If you have already installed the analyser, you can use

```shell
$ stan --help
```

to get the short information of all possible commands and options in
your terminal.

#### Main command

The main command is the one that actually would analyse the Haskell
codebase. There are plenty of configurations and options you can tune
for each run (similarly to the TOML configurations):

- Specify the [HIE files][hie] folder (will use `.hie/` otherwise)
- Specify `.cabal` files of your project (will lookup automatically
  otherwise)
- Turn on/off the usage of the default `.stan.toml` configuration file
- Specify the [TOML][toml] configuration file to use (will be used
  additionally to default TOML file if applicable)
- Filter in or out specific files, directories, inspections,
  categories or severities
- Generate the HTML report file
- Set up the output verbosity

More precisely the commands and options are described in here:

```
stan
    [REPORT]
    [   CHECKs {[TYPE option] [FILTER option] [SCOPE option]}
      | REMOVEs {SCOPE option}
      | IGNOREs {ID option}
    ]
    [--hiedir=DIR_PATH]
    [--cabal-file-path=FILE_PATHs]
    [--config-file=FILE_PATH]
    [--no-default]
    [-s|--short]
    [--hide-solution]
    [-h|--help]
    [-v|--version]

Description:
  CHECKs           Command to Specify the list of checks
  REMOVEs          Command to Specify scope to be removed
  IGNOREs          Command to Specify the list of what needs to be ignored
  REPORT           Command to generate an HTML Report
  --hiedir=DIR_PATH        Relative path to the directory with HIE
                           files (default: .hie)
  --cabal-file-path=FILE_PATHs
                           Relative path to the .cabal file (can specify many of this option)
  --config-file=FILE_PATH  Relative path to the .toml configurations file
  --no-default             Ignore local .stan.toml configuration file
  -s,--short               Hide verbose output information for observations
  --hide-solution          Hide verbose solution information for observations
  -h,--help                Show this help text
  -v,--version             Show Stan's version

Sub-commands options:


  TYPE:
    --include                Include check
    --exclude                Exclude check
  FILTER:
    --id=INSPECTION_ID       Inspection ID to be used
    --severity=SEVERITY      Inspection Severity to exclude or include
    --category=CATEGORY      Inspection Category to exclude or include
    --filter-all             Exclude or include ALL inspections
  SCOPE:
    --file=FILE_PATH         File to exclude or include
    --directory=DIRECTORY_PATH
                           Directory to exclude or include
    --scope-all              Apply check to all files
```

For example, if you want to run Stan analysis only on a single file,
you can use the following command:

```shell
$ stan check --exclude --filter-all --scope-all \
       check --include --filter-all --file=src/Stan/Example.hs
```

#### Inspections

You can find the list of all available inspections with description
and additional information on our
[dedicated wiki page][inspections]. However, with the tool you can get
this information easily by using the `inspection` command. Optionally,
you can see details of a particular inspection by typing the
corresponding inspection ID alongside. You can see more robust
description of the command here:

```
inspection – Show all Inspections

Usage:
  stan inspection [INSPECTION_ID]

Available options:
  INSPECTION_ID            Show specific Inspection information
  -h,--help                Show this help text
```

#### Converting between TOML and CLI configurations

It is usually convenient to have a proper configuration file that
suits your project, which you can reuse each run of the Stan.

But sometimes you need to quickly run the tool with the same settings
on another machine where having such files is not possible. Or you
want to send the reproducible command, that anyone could execute and
get the identical results. For these purposes, we have a special
command that allows you to do so:

```
toml-to-cli – Convert TOML configuration file into stan CLI command

Usage:
    stan toml-to-cli [--config-file=FILE_PATH]

Available options:
  --config-file=FILE_PATH  Relative path to the .toml configurations file
  -h,--help                Show this help text
```

And for convenience you are able to use the reversed command –– `cli-to-toml`.

```
cli-to-toml – Convert CLI arguments into stan TOML configuration

Usage:
    stan cli-to-toml
      [--config-file=FILE_PATH]
      [   CHECKs {[TYPE option] [FILTER option] [SCOPE option]}
        | REMOVEs {SCOPE option}
        | IGNOREs {ID option}
      ]
```

## Links to wiki

* [Glossary][glossary]
* [All inspections][inspections]
* [For developers][development]
* [Useful links][links]
* [GHC issues backlog][ghc-backlog]


[inspections]: https://github.com/kowainik/stan/wiki/All-Inspections
[development]: https://github.com/kowainik/stan/wiki/Development
[ghc-backlog]: https://github.com/kowainik/stan/wiki/GHC-Backlog
[glossary]: https://github.com/kowainik/stan/wiki/Glossary
[links]: https://github.com/kowainik/stan/wiki/Useful-links
[hie]: https://gitlab.haskell.org/ghc/ghc/-/wikis/hie-files
[toml]: https://github.com/toml-lang/toml
