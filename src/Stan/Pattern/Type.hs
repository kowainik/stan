{-# LANGUAGE CPP #-}

{- HLINT ignore "Avoid lambda using `infix`" -}

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

      -- ** Textual types
    , charPattern
    , stringPattern
    , textPattern

      -- * Foldable patterns
    , foldableTypesPatterns
    , foldableMethodsPatterns
    ) where

import Stan.NameMeta (NameMeta (..), baseNameFrom, ghcPrimNameFrom, primTypeMeta, textNameFrom, _nameFrom)
import Stan.Pattern.Edsl (PatternBool (..))


{- | Query pattern used to search types in HIE AST.
-}
data PatternType
    {- | Argument, type or constructor:

    +---------------------+---------------------------------------------------------------------+
    | @a@                 | @PatternName (NameMeta ... \"a\") []@                               |
    +---------------------+---------------------------------------------------------------------+
    | @[a]@               | @PatternName (NameMeta ... \"List\") [aPattern]@ (after GHC 9.6)    |
    |                     | @PatternName (NameMeta ... \"[]\") [aPattern]@   (before GHC 9.6)   |
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
#if __GLASGOW_HASKELL__ >= 910
    "String" `_nameFrom` "GHC.Internal.Base"
#else
    "String" `_nameFrom` "GHC.Base"
#endif
    |:: []
  where
    listNameMeta :: NameMeta
#if __GLASGOW_HASKELL__ < 906
    listNameMeta = primTypeMeta "[]"
#elif __GLASGOW_HASKELL__ >= 906
    listNameMeta = primTypeMeta "List"
#endif

-- | 'PatternType' for 'NonEmpty'.
nonEmptyPattern :: PatternType
nonEmptyPattern =
#if __GLASGOW_HASKELL__ >= 910
  "NonEmpty" `_nameFrom` "GHC.Internal.Base"
#else
  "NonEmpty" `_nameFrom` "GHC.Base"
#endif
  |:: [ (?) ]

-- | 'PatternType' for @[a] -> _@ or @String -> _@.
listFunPattern :: PatternType
listFunPattern = listPattern |-> (?)

-- The source for integerPattern and naturalPattern varies depending on the GHC
-- version
#if __GLASGOW_HASKELL__ < 900

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

#elif __GLASGOW_HASKELL__ >= 900

-- | 'PatternType' for 'Integer'.
integerPattern :: PatternType
integerPattern = NameMeta
    { nameMetaName       = "Integer"
    , nameMetaModuleName = "GHC.Num.Integer"
    , nameMetaPackage    = "ghc-bignum"
    } |:: []

-- | 'PatternType' for 'Natural'.
naturalPattern :: PatternType
naturalPattern = NameMeta
    { nameMetaName       = "Natural"
    , nameMetaModuleName = "GHC.Num.Natural"
    , nameMetaPackage    = "ghc-bignum"
    } |:: []

#endif

charPattern :: PatternType
charPattern = primTypeMeta "Char" |:: []

-- | 'PatternType' for 'String'.
stringPattern :: PatternType
stringPattern =
#if __GLASGOW_HASKELL__ >= 910
  "String" `_nameFrom` "GHC.Internal.Base"
#else
  "String" `_nameFrom` "GHC.Base"
#endif
  |:: []

-- | 'PatternType' for 'Text'.
textPattern :: PatternType
textPattern = "Text" `textNameFrom` "Data.Text.Internal" |:: []

----------------------------------------------------------------------------
-- Section of Foldable patterns
----------------------------------------------------------------------------

-- | List of types for @STAN-0207@.
foldableTypesPatterns :: NonEmpty PatternType
foldableTypesPatterns = maybePattern :| [eitherPattern, pairPattern]

-- | 'PatternType' for 'Maybe'
maybePattern :: PatternType
maybePattern =
#if __GLASGOW_HASKELL__ >= 910
  "Maybe" `_nameFrom` "GHC.Internal.Maybe"
#else
  "Maybe" `_nameFrom` "GHC.Maybe"
#endif
  |:: [ (?) ]

-- | 'PatternType' for 'Either'
eitherPattern :: PatternType
eitherPattern =
#if __GLASGOW_HASKELL__ >= 910
  "Either" `_nameFrom` "GHC.Internal.Data.Either"
#else
  "Either" `_nameFrom` "Data.Either"
#endif
  |:: [ (?), (?) ]

-- | 'PatternType' for pair @(,)@.
pairPattern :: PatternType
#if __GLASGOW_HASKELL__ < 908
pairPattern = "(,)" `ghcPrimNameFrom` ghcTuple |:: [ (?), (?) ]
#elif __GLASGOW_HASKELL__ >= 908
pairPattern = "Tuple2" `ghcPrimNameFrom` ghcTuple |:: [ (?), (?) ]
#endif
  where
#if __GLASGOW_HASKELL__ < 906
    ghcTuple = "GHC.Tuple"
#elif __GLASGOW_HASKELL__ < 910
    ghcTuple = "GHC.Tuple.Prim"
#else
    ghcTuple = "GHC.Tuple"
#endif

{- | Type patterns for the 'Foldable' typeclass methods. Represented
as a non-empty list of pairs:

* Method name
* Function from type to pattern (where things like 'Maybe', 'Either'
  should be)
-}
foldableMethodsPatterns :: NonEmpty (NameMeta, PatternType -> PatternType)
foldableMethodsPatterns =
      method "fold"     `ofType` (\t -> t |-> (?)) :|
    [ method "foldMap"  `ofType` \t -> (?) |-> t |-> (?)
    , method "foldMap'" `ofType` \t -> (?) |-> t |-> (?)
    , method "foldr"    `ofType` \t -> (?) |-> (?) |-> t |-> (?)
    , method "foldr'"   `ofType` \t -> (?) |-> (?) |-> t |-> (?)
    , method "foldl"    `ofType` \t -> (?) |-> (?) |-> t |-> (?)
    , method "foldl'"   `ofType` \t -> (?) |-> (?) |-> t |-> (?)
    , method "foldr1"   `ofType` \t -> (?) |-> t |-> (?)
    , method "foldl1"   `ofType` \t -> (?) |-> t |-> (?)
    , method "toList"   `ofType` \t -> t |-> (?)
    , method "null"     `ofType` \t -> t |-> (?)
    , method "length"   `ofType` \t -> t |-> (?)
    , method "elem"     `ofType` \t -> (?) |-> t |-> (?)
    , method "maximum"  `ofType` \t -> t |-> (?)
    , method "minimum"  `ofType` \t -> t |-> (?)
    , method "sum"      `ofType` \t -> t |-> (?)
    , method "product"  `ofType` \t -> t |-> (?)
    ]
  where
    ofType :: a -> b -> (a, b)
    ofType = (,)

    method :: Text -> NameMeta
    method name =
#if __GLASGOW_HASKELL__ >= 910
      name `_nameFrom` "GHC.Internal.Data.Foldable"
#else
      name `_nameFrom` "Data.Foldable"
#endif
