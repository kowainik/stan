# Stan

<p align="center"><img alt="Stan Logo" src="https://user-images.githubusercontent.com/8126674/83521583-59383080-a4d7-11ea-8d9e-33be4677ecb3.png" width=300px height=300px/></p>

[![GitHub CI](https://github.com/kowainik/stan/workflows/CI/badge.svg)](https://github.com/kowainik/stan/actions)
[![Hackage](https://img.shields.io/hackage/v/stan.svg)](https://hackage.haskell.org/package/stan)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/stan/blob/main/LICENSE)

Stan is a Haskell **ST**atic **AN**alysis tool.

> ⚠️ Note: Stan is in the beta phase. The API is the subject to be
> changed if required by our needs ⚠️

## Table of Contents

 * [What this tool is about](#what-this-tool-is-about)
 * [Goals](#goals)
 * [Features](#features)
 * [How it works](#how-it-works)
 * [Installation instructions](#installation-instructions)
    * [Using Cabal](#using-cabal)
    * [Using Stack](#using-stack)
    * [Hackage](#hackage)
    * [Homebrew](#homebrew)
    * [Ubuntu PPA](#ubuntu-ppa)
    * [Download binary](#download-binary)
 * [Usage instructions](#usage-instructions)
    * [General configuration info](#general-configuration-info)
    * [TOML configurations](#toml-configurations)
    * [Command-line Interface](#command-line-interface)
       * [Main command](#main-command)
       * [Inspections](#inspections)
       * [Converting between TOML and CLI configurations](#converting-between-toml-and-cli-configurations)
 * [Other tools](#other-tools)
 * [Roadmap](#roadmap)
 * [Users](#users)
 * [Links to Wiki](#links-to-wiki)

## What this tool is about

[[Back to the Table of Contents] ↑](#table-of-contents)

Stan is a command-line tool for analysing Haskell projects.
It discovers which parts of the code can potentially be improved,
and offers suggestions on how to do so.
Stan is searching for not only
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

For a crash course to Stan, watch the talk about Stan, presented by
Veronika Romashkina and Dmitrii Kovanikov at the Haskell Love
conference.

[![Stan – Haskell Static Analyser](https://img.youtube.com/vi/wb5PLv6-e6I/0.jpg)](https://www.youtube.com/watch?v=wb5PLv6-e6I)

## Goals

[[Back to the Table of Contents] ↑](#table-of-contents)

Stan design and implementation is driven by the following goals:

- Catch common errors, anti-patterns, performance issues
- Provide meaningful insights on the projects generally
- Point out potential bugs and weak points in the programs flow for
  users, so they can carefully evaluate each problem with the code
- Help beginners to learn best practices in an easy and informative way
- Generate the report that can be used as a proof of code quality
- Create best in the class and flexible enough interface for usage
  (including e.g. opt-in and opt-out inspections)

## Features

[[Back to the Table of Contents] ↑](#table-of-contents)

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

[[Back to the Table of Contents] ↑](#table-of-contents)

Stan analysis is based on the [HIE files][hie] — compile-time
information about Haskell source code gathered and recorded by
GHC. The HIE files contain the Haskell AST, detailed information about
each identifier and types of all expressions and sub-expressions. GHC
does a huge amount of work when compiling the Haskell projects, and
Stan takes advantage of this feature to avoid duplicating the work and
focus more on the unique features.

To analyse HIE files easily, we developed an eDSL for defining AST and
Type patterns based on the
[_final tagless_](http://okmij.org/ftp/tagless-final/course/lecture.pdf)
approach. Stan algorithm traverses HIE AST for each HIE file in the
project, and matches every AST node with the given pattern to find
potential improvement areas in the code.

Each Stan analysis check is represented by the __inspection__ with the
unique ID. Each inspection has a name, description, __severity__, list
of __categories__, pattern for matching relevant parts of source code
and possible solutions to the problem.

When an inspection is casted on the project, it produces zero or more
__observations__ —.
You can think of an _observation_ as a pair of an inspection and
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

[[Back to the Table of Contents] ↑](#table-of-contents)

Stan takes advantage of the
[GHC API](http://hackage.haskell.org/package/ghc) to provide its
analysis. Because of this, Stan and the analysed project need to be
built with the same GHC version (for more details see
[#178](https://github.com/kowainik/stan/issues/178)). That is why the
easiest and most robust way to install Stan is to build it from
sources on your machine.

> __Note:__ Stan is compatible with the GHC versions ⩾ 8.8

### Using Cabal

[[Back to the Table of Contents] ↑](#table-of-contents)

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
$ cp "$(cabal v2-exec --verbose=0 --offline sh -- -c 'command -v stan')" ~/.local/bin/stan
```

### Using Stack

[[Back to the Table of Contents] ↑](#table-of-contents)

Below are the steps to install Stan from its repository using the Stack tool.

> You need to have [Stack ⩾ 2.1.3](http://haskellstack.org)

First, you need to clone the repository and change to the `stan` directory:

```shell
$ git clone https://github.com/kowainik/stan.git
$ cd stan
```

Then, using Stack, you need to build the package and copy the executable to the
desired location (typically one on your PATH). If Stack's `--local-bin-path`
option is omitted, Stack will copy the built executable to a
[default location](https://docs.haskellstack.org/en/stable/yaml_configuration/#local-bin-path):

```shell
$ stack --local-bin-path=<path_to_desired_location> install
```

Stack's build, including the version of GHC used, will be configured by a
`stack.yaml` file provided by the repository or the package from Hackage. If you
wish to build Stan with a different version of GHC than that assumed, you will
need to edit the configuration accordingly.

### Hackage

[[Back to the Table of Contents] ↑](#table-of-contents)

Stan is available on Hackage.

Using the Cabal build tool, you can install the tool from there as well:

```shell
$ cabal v2-install stan --install-method=copy --overwrite-policy=always
```

You can also choose with which GHC version you want to have Stan installed, and
optionally add some suffix to the executable name:

```shell
$ cabal v2-install stan \
    -w ghc-8.10.1 \
    --install-method=copy \
    --overwrite-policy=always \
    --program-suffix=-8.10.1
```

Using the Stack tool, you can also install a version of Stan from Hackage:

First, you need to unpack the package locally and change to the package's
directory. The following example assumes `stan-0.1.0.1`:

```shell
$ stack unpack stan-0.1.0.1 # Specify 'stan' for the most recent version
$ cd stan-0.1.0.1 # The directory is named after the package version
```

Then, using Stack, you need to build the package and copy the executable to the
desired location (as above, for the repository example):

```shell
$ stack --local-bin-path=<path_to_desired_location> install
```

### Homebrew

[[Back to the Table of Contents] ↑](#table-of-contents)

If you are on MacOS, you can get Stan using Homebrew Kowainik's Tap.

You need to run the following command for that:

```shell
$ brew install kowainik/tap/stan
```

> NOTE: Homebrew installs the Stan version build with the latest supported GHC
> version. This means that this version of Stan is working with the project with
> the same GHC version due to the GHC issues described above.

### Ubuntu PPA

[[Back to the Table of Contents] ↑](#table-of-contents)

If you are on Ubuntu, you can get Stan using
[Kowainik's PPA](https://launchpad.net/~kowainik/+archive/ubuntu/stan).

You need to run the following commands for that:

```shell
$ sudo add-apt-repository ppa:kowainik/stan
$ sudo apt update
$ sudo apt install stan
```

> NOTE: `apt-get` installs the Stan version build with the latest supported GHC
> version. This means that this version of Stan is working with the project with
> the same GHC version due to the GHC issues described above.

### Download binary

[[Back to the Table of Contents] ↑](#table-of-contents)

You can download binary directly
[from GitHub releases](https://github.com/kowainik/stan/releases/latest).

After downloading binary, make it executable and copy it under
convenient location, e.g.:

```shell
$ chmod +x stan-0.0.1.0-Linux-ghc-8.10.1
$ mv stan-0.0.1.0-Linux-ghc-8.10.1 ~/.local/bin/stan
```

> NOTE: you need to download binary for your specific OS and
> specicific GHC version you use due to the GHC issues described above.

## Usage instructions

[[Back to the Table of Contents] ↑](#table-of-contents)

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

to see all found suggestions in your terminal.

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

[[Back to the Table of Contents] ↑](#table-of-contents)

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

[[Back to the Table of Contents] ↑](#table-of-contents)

Stan supports [TOML][toml] runtime configuration in order to customize
the work of the tool based on the user's individual requirements. You
can use the TOML configuration to disable some inspections, enable
them only in particular Haskell modules, ignore some observations or
completely remove some files from the analysis.

Specifically, you can use the following variables to set up custom configurations with TOML:

| Variable | Description | Examples |
|----------|-------------|----------|
| `check` | Set up rules to control the set of inspections per scope. | `check = [{type = "Exclude", id = "STAN-0101", scope = "all"}]`            |
| `remove` | Remove some files from the analysis completely. Stan won't be run in the specified scope at all. | `remove = [ {file = "src/File.hs"}, {directory = "folder/"} ]` |
| `ignore` | Ignore specific observation that was found in your project   | `ignore = [{ id = "OBS-STAN-0001-YrzpQi-11:42" }]` |

See [Haddock documentation](http://hackage.haskell.org/package/stan-0.0.0.0/docs/Stan-Config.html#g:5)
for explanation of how the TOML
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

[[Back to the Table of Contents] ↑](#table-of-contents)

This section describes what is possible to achieve with the Stan
CLI. If you have already installed the analyser, you can use

```shell
$ stan --help
```

to get the short information of all possible commands and options in
your terminal.

#### Main command

[[Back to the Table of Contents] ↑](#table-of-contents)

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
- Choose to have machine readable JSON output instead

Here is the high-level explanation of the available sub-commands:

| Sub-command | Description | Examples |
|-------------|-------------|----------|
| `check` | Set up rules to control the set of inspections per scope. | `stan check --exclude --category=Infinity --scope-all check --include --id "STAN-0101" --file=src/File.hs` |
| `remove` | Remove some files from the analysis completely. Stan won't be run in the specified scope at all. | `stan remove --file=src/File.hs remove --directory=folder/`         |
| `ignore` | Ignore specific observation that was found in your project   | `stan ignore --id "OBS-STAN-0001-YrzpQi-11:42"`          |

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
    [--json-output]
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
  --json-output            Output the machine-readable output in JSON format instead
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

Report options:

  -b,--browse              Open report in a browser
```

For example, if you want to run Stan analysis only on a single file, generate
the HTML report and immediately open report in a browser, you can use
the following command:

```shell
$ stan check --exclude --filter-all --scope-all \
       check --include --filter-all --file=src/Stan/Example.hs \
       report --browse
```

#### Inspections

[[Back to the Table of Contents] ↑](#table-of-contents)

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

[[Back to the Table of Contents] ↑](#table-of-contents)

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

## Other tools

[[Back to the Table of Contents] ↑](#table-of-contents)

* [GHC](https://www.haskell.org/ghc/) — __Glasgow Haskell Compiler__

  GHC is the most popular Haskell compiler. As it has access to all steps of the
  code compilation, GHC can warn about different aspects of your code:
  non-exhaustive pattern matching, unused variables, etc.

  However, it is not supposed to be used as a static analysis tool. It provides
  errors and warnings as a part of the whole compilation pipeline.

* [Weeder](https://github.com/ocharles/weeder) — __Haskell dead-code analysis tool__

  Weeder is a tool that analyses the code but in a very specific and limited
  case. It helps to eliminate unreachable code in your project. Similarly to
  Stan, the Weeder tool is also working with the HIE files to get this
  information.

* [HLint](https://github.com/ndmitchell/hlint) — __Haskell Linter Tool__

  HLint is a linter tool that suggests code improvements to make code simpler.

  Unlike Stan, that uses the HIE files for analysis and accesses the complete
  compile-time info produced by GHC, HLint relies only on parsing, which has its
  own benefits but also limits its capabilities.

  Stan and HLint are complementary tools that have different scopes and goals.
  There is no intention to duplicate HLint in Stan.

To learn more about the implementation and goals of our project, please read the
sections above that describe the Stan project in detail.

## Roadmap

[[Back to the Table of Contents] ↑](#table-of-contents)

Our plan for the nearest future:

 - [ ] Opt-in inspections
 - [ ] Custom users' inspections
 - [ ] More inspections on potential bugs and performance
 - [ ] Single-pass traverse on AST

We have much more ideas to work on.
See more detailed plan in the dedicated [GitHub Project page](https://github.com/kowainik/stan/projects/1).

## Users

Stan is known to be adopted by the following companies:

* A major international bank
* [ITProTV](https://www.itpro.tv/)
* [Soisy](https://www.soisy.it/)

## Links to Wiki

[[Back to the Table of Contents] ↑](#table-of-contents)

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
