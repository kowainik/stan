{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Some `Stan.Inspection.Inspection`s require to know about types and some
mechanism to match types to the given 'Pattern'. This information on types/type
expressions is taken from @HIE files@ in a more suitable view.

Let's take a look at the function @foo@:

@
foo :: NonEmpty String -> Int
@

In @HIE@ files it will be stored as an 'Array' like this:

@
  1 -> "Int"      []
  2 -> "String"   []
  3 -> "NonEmpty" [ 2 ]
  4 -> FunType    3 1
@

This module contains an implementation of the process of retrieval of this
information from there.
-}

module Stan.Hie.Match
    ( Pattern (..)
    , hieMatchPattern

      -- * Common 'Pattern's
    , listPattern
    , listFunPattern
    ) where

import BasicTypes (PromotionFlag (NotPromoted))
import Data.Array (Array)
import HieTypes (HieArgs (..), HieType (..), HieTypeFlat, TypeIndex)
import IfaceType (IfaceTyCon (..), IfaceTyConInfo (..))

import Stan.NameMeta (NameMeta (..), compareNames)

import qualified Data.Array as Arr


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
    deriving stock (Show, Eq)

{- | Matching function that searches the array of types recursively.
-}
hieMatchPattern
    :: Array TypeIndex HieTypeFlat  -- ^ Array of all types in HIE file
    -> Pattern  -- ^ Our search query
    -> TypeIndex   -- ^ Index of the current expression type
    -> Bool  -- ^ If matched type is found
hieMatchPattern arr pat i = curFlat `satisfyPattern` pat
  where
    curFlat :: HieTypeFlat
    curFlat = arr Arr.! i

    match :: Pattern -> TypeIndex -> Bool
    match = hieMatchPattern arr

    satisfyPattern :: HieTypeFlat -> Pattern -> Bool
    satisfyPattern _ PatternAnything = True
    satisfyPattern (HTyVarTy name) (PatternName nameMeta []) =
        compareNames nameMeta name
    satisfyPattern
        (HTyConApp IfaceTyCon{..} (HieArgs hieArgs))
        (PatternName nameMeta args)
      =
        ifaceTyConIsPromoted ifaceTyConInfo == NotPromoted
        && compareNames nameMeta ifaceTyConName
        && checkWith (\(_, ix) a -> match a ix) hieArgs args
    satisfyPattern (HFunTy i1 i2) (PatternFun p1 p2) =
           match p1 i1
        && match p2 i2
    satisfyPattern (HQualTy _ ix) p = match p ix
    satisfyPattern (HForAllTy _ ix) p = match p ix
    satisfyPattern _flat _p = False

    checkWith :: (a -> b -> Bool) -> [a] -> [b] -> Bool
    checkWith _ [] []         = True
    checkWith _ [] _          = False
    checkWith _ _ []          = False
    checkWith f (a:as) (b:bs) = f a b && checkWith f as bs


-- | 'Pattern' for list @[a]@.
listPattern :: Pattern
listPattern = PatternName
    ( NameMeta
        { nameMetaName       = "[]"
        , nameMetaModuleName = "GHC.Types"
        , nameMetaPackage    = "ghc-prim"
        }
    )
    [PatternAnything]

listFunPattern :: Pattern
listFunPattern = PatternFun listPattern PatternAnything
