{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Some 'Stan.Inspection.Inspection's require to know about types and some
mechanism to match types to the given 'PatternType'. This information on types/type
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

module Stan.Hie.MatchType
    ( hieMatchPatternType
    ) where

import BasicTypes (PromotionFlag (NotPromoted))
import Data.Array (Array)
import HieTypes (HieArgs (..), HieType (..), HieTypeFlat, TypeIndex)
import IfaceType (IfaceTyCon (..), IfaceTyConInfo (..))

import Stan.Core.List (checkWith)
import Stan.NameMeta (compareNames)
import Stan.Pattern.Type (PatternType (..))

import qualified Data.Array as Arr


{- | Matching function that searches the array of types recursively.
-}
hieMatchPatternType
    :: Array TypeIndex HieTypeFlat  -- ^ Array of all types in HIE file
    -> PatternType  -- ^ Our search query
    -> TypeIndex   -- ^ Index of the current expression type
    -> Bool  -- ^ If matched type is found
hieMatchPatternType arr pat i = curFlat `satisfyPattern` pat
  where
    curFlat :: HieTypeFlat
    curFlat = arr Arr.! i

    match :: PatternType -> TypeIndex -> Bool
    match = hieMatchPatternType arr

    satisfyPattern :: HieTypeFlat -> PatternType -> Bool
    satisfyPattern _ PatternTypeAnything = True
    satisfyPattern t (PatternTypeOr p1 p2) =
           satisfyPattern t p1
        || satisfyPattern t p2
    satisfyPattern (HTyVarTy name) (PatternTypeName nameMeta []) =
        compareNames nameMeta name
    satisfyPattern
        (HTyConApp IfaceTyCon{..} (HieArgs hieArgs))
        (PatternTypeName nameMeta args)
      =
        ifaceTyConIsPromoted ifaceTyConInfo == NotPromoted
        && compareNames nameMeta ifaceTyConName
        && checkWith (\(_, ix) a -> match a ix) hieArgs args
    satisfyPattern (HFunTy i1 i2) (PatternTypeFun p1 p2) =
           match p1 i1
        && match p2 i2
    satisfyPattern (HQualTy _ ix) p = match p ix
    satisfyPattern (HForAllTy _ ix) p = match p ix
    satisfyPattern _flat _p = False
