{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Patterns for types and type search.
-}

module Stan.Pattern
    ( -- * Type
      Pattern (..)

      -- * eDSL
    , (?)
    , (|||)
    , (|->)
    , (|::)

      -- * Common 'Pattern's
    , listPattern
    , nonEmptyPattern
    , listFunPattern
    , integerPattern
    , naturalPattern
    ) where

import Stan.NameMeta (NameMeta (..), baseNameFrom)


{- | Query pattern used to search types in HIE AST.
-}
data Pattern
    {- | Argument, type or constructor:

    +---------------------+---------------------------------------------------------------------+
    | @a@                 | @PatternName (NameMeta ... \"a\") []@                               |
    +---------------------+---------------------------------------------------------------------+
    | @[a]@               | @PatternName (NameMeta ... \"[]\") [aPattern]@                      |
    +---------------------+---------------------------------------------------------------------+
    | @Either Int String@ | @PatternName (NameMeta ... \"Either\") [intPattern, stringPattern]@ |
    +---------------------+---------------------------------------------------------------------+
    -}
    = PatternName NameMeta [Pattern]
    -- | Function pattern.
    | PatternFun Pattern Pattern
    -- | Type wildcard, matches anything.
    | PatternAnything
    -- | Choice between patterns. Should match either of them.
    | PatternOr Pattern Pattern
    deriving stock (Show, Eq)

-- | Short operator alias for 'PatternAnything'.
(?) :: Pattern
(?) = PatternAnything

-- | Short operator alias for 'PatternOr'.
infixr 3 |||
(|||) :: Pattern -> Pattern -> Pattern
(|||) = PatternOr

-- | Short operator alias for 'PatternFun'.
infixr 4 |->
(|->) :: Pattern -> Pattern -> Pattern
(|->) = PatternFun

-- | Short operator alias for 'PatternName'.
infix 5 |::
(|::) :: NameMeta -> [Pattern] -> Pattern
(|::) = PatternName

-- | 'Pattern' for list @[a]@ or @'String'@.
listPattern :: Pattern
listPattern =
    listNameMeta |:: [ (?) ]
    |||
    "String" `baseNameFrom` "GHC.Base" |:: []
  where
    listNameMeta :: NameMeta
    listNameMeta = NameMeta
        { nameMetaName       = "[]"
        , nameMetaModuleName = "GHC.Types"
        , nameMetaPackage    = "ghc-prim"
        }

-- | 'Pattern' for 'NonEmpty'.
nonEmptyPattern :: Pattern
nonEmptyPattern = "NonEmpty" `baseNameFrom` "GHC.Base" |:: [ (?) ]

-- | 'Pattern' for @[a] -> _@ or @String -> _@.
listFunPattern :: Pattern
listFunPattern = listPattern |-> (?)

-- | 'Pattern' for 'Integer'.
integerPattern :: Pattern
integerPattern = NameMeta
    { nameMetaName       = "Integer"
    , nameMetaModuleName = "GHC.Integer.Type"
    , nameMetaPackage    = "integer-wired-in"
    } |:: []

-- | 'Pattern' for 'Natural'.
naturalPattern :: Pattern
naturalPattern = "Natural" `baseNameFrom` "GHC.Natural" |:: []
