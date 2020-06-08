{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Patterns for types and type search.
-}

module Stan.Pattern.Type
    ( -- * Type
      PatternType (..)

      -- * eDSL
    , (|->)
    , (|::)

      -- * Common 'PatternType's
    , listPattern
    , nonEmptyPattern
    , listFunPattern
    , integerPattern
    , naturalPattern
    ) where

import Stan.NameMeta (NameMeta (..), baseNameFrom)
import Stan.Pattern.Edsl (PatternBool (..))


{- | Query pattern used to search types in HIE AST.
-}
data PatternType
    {- | Argument, type or constructor:

    +---------------------+---------------------------------------------------------------------+
    | @a@                 | @PatternName (NameMeta ... \"a\") []@                               |
    +---------------------+---------------------------------------------------------------------+
    | @[a]@               | @PatternName (NameMeta ... \"[]\") [aPattern]@                      |
    +---------------------+---------------------------------------------------------------------+
    | @Either Int String@ | @PatternName (NameMeta ... \"Either\") [intPattern, stringPattern]@ |
    +---------------------+---------------------------------------------------------------------+
    -}
    = PatternTypeName !NameMeta ![PatternType]
    -- | Function pattern.
    | PatternTypeFun !PatternType !PatternType
    -- | Type wildcard, matches anything.
    | PatternTypeAnything
    -- | Choice between patterns. Should match either of them.
    | PatternTypeOr !PatternType !PatternType
    -- | Union of patterns. Should match both of them.
    | PatternTypeAnd !PatternType !PatternType
    -- | Negation of pattern. Should match everything except this pattern.
    | PatternTypeNeg !PatternType
    deriving stock (Show, Eq)

instance PatternBool PatternType where
    (?) :: PatternType
    (?) = PatternTypeAnything

    neg :: PatternType -> PatternType
    neg = PatternTypeNeg

    (|||) :: PatternType -> PatternType -> PatternType
    (|||) = PatternTypeOr

    (&&&) :: PatternType -> PatternType -> PatternType
    (&&&) = PatternTypeAnd

-- | Short operator alias for 'PatternFun'.
infixr 4 |->
(|->) :: PatternType -> PatternType -> PatternType
(|->) = PatternTypeFun

-- | Short operator alias for 'PatternTypeName'.
infix 5 |::
(|::) :: NameMeta -> [PatternType] -> PatternType
(|::) = PatternTypeName

-- | 'PatternType' for list @[a]@ or @'String'@.
listPattern :: PatternType
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

-- | 'PatternType' for 'NonEmpty'.
nonEmptyPattern :: PatternType
nonEmptyPattern = "NonEmpty" `baseNameFrom` "GHC.Base" |:: [ (?) ]

-- | 'PatternType' for @[a] -> _@ or @String -> _@.
listFunPattern :: PatternType
listFunPattern = listPattern |-> (?)

-- | 'PatternType' for 'Integer'.
integerPattern :: PatternType
integerPattern = NameMeta
    { nameMetaName       = "Integer"
    , nameMetaModuleName = "GHC.Integer.Type"
    , nameMetaPackage    = "integer-wired-in"
    } |:: []

-- | 'PatternType' for 'Natural'.
naturalPattern :: PatternType
naturalPattern = "Natural" `baseNameFrom` "GHC.Natural" |:: []
