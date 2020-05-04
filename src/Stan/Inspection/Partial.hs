{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains all 'Inspection's for partial functions.
-}

module Stan.Inspection.Partial
    ( -- * Partial 'Inspection's
      -- *** Partial 'GHC.List.head'
      stan0001
      -- *** Partial 'GHC.List.tail'
    , stan0002
      -- *** Partial 'GHC.List.init'
    , stan0003
      -- *** Partial 'GHC.List.last'
    , stan0004
      -- *** Partial 'GHC.List.!!'
    , stan0005
      -- *** Partial 'GHC.List.cycle'
    , stan0006
      -- *** Partial 'Data.OldList.genericIndex'
    , stan0007
      -- *** Partial 'Data.Maybe.fromJust'
    , stan0008
      -- *** Partial 'Text.Read.read'
    , stan0009
      -- *** Partial 'GHC.Enum.succ'
    , stan0010
      -- *** Partial 'GHC.Enum.pred'
    , stan0011
      -- *** Partial 'GHC.Enum.toEnum'
    , stan0012
      -- *** Partial 'Data.Foldable.maximum'
    , stan0013
      -- *** Partial 'Data.Foldable.minimum'
    , stan0014
      -- *** Partial 'Data.Foldable.maximumBy'
    , stan0015
      -- *** Partial 'Data.Foldable.minimumBy'
    , stan0016
      -- *** Partial 'Data.Foldable.foldl1'
    , stan0017
      -- *** Partial 'Data.Foldable.foldl1\''
    , stan0018
      -- *** Partial 'Data.Foldable.foldr1'
    , stan0019
      -- *** Partial instance for 'NonEmpty' method 'GHC.Exts.fromList'
    , stan0020
      -- *** Partial instance for 'Natural' method 'GHC.Num.fromInteger'
    , stan0021

      -- * List of all partial 'Inspection's
    , partialInspectionsMap
    ) where

import Relude.Extra.Lens (set, (%~), (.~))
import Relude.Extra.Tuple (mapToFst)

import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..), InspectionAnalysis (..), InspectionsMap, categoryL,
                        descriptionL, solutionL)
import Stan.NameMeta (NameMeta (..), mkBaseFoldableMeta, mkBaseListMeta, mkBaseMeta,
                      mkBaseOldListMeta, moduleNameL)
import Stan.Pattern (Pattern (..), integerPattern, listFunPattern, listPattern, naturalPattern,
                     nonEmptyPattern, (?), (|->))
import Stan.Severity (Severity (..))

import qualified Stan.Category as Category


-- | All partial 'Inspection's.
partialInspectionsMap :: InspectionsMap
partialInspectionsMap = fromList $ map (mapToFst inspectionId)
    [ stan0001
    , stan0002
    , stan0003
    , stan0004
    , stan0005
    , stan0006
    , stan0007
    , stan0008
    , stan0009
    , stan0010
    , stan0011
    , stan0012
    , stan0013
    , stan0014
    , stan0015
    , stan0016
    , stan0017
    , stan0018
    , stan0019
    , stan0020
    , stan0021
    ]

-- | Smart constructor to create generic partial 'Inspection' with a given 'Pattern'.
mkPartialInspectionPattern
    :: Id Inspection
    -> NameMeta
    -> Pattern
    -> Text  -- ^ Type name
    -> Inspection
mkPartialInspectionPattern insId nameMeta@NameMeta{..} pat typeName = Inspection
    { inspectionId = insId
    , inspectionName = "Partial: " <> nameMetaPackage <> "/" <> nameMetaName
    , inspectionDescription = usage nameMetaName typeName
    , inspectionSolution = []
    , inspectionCategory = one Category.partial
    , inspectionSeverity = Warning
    , inspectionAnalysis = FindName nameMeta pat
    }

-- | Smart constructor to create generic partial 'Inspection' with 'PatternAnything'.
mkPartialInspection
    :: Id Inspection
    -> NameMeta
    -> Text  -- ^ Type name
    -> Inspection
mkPartialInspection insId nameMeta = mkPartialInspectionPattern insId nameMeta PatternAnything

usage :: Text -> Text -> Text
usage funName forWhat =
    "Usage of partial function '" <> funName <> "' for " <> forWhat

{- | Smart constructor to create partial 'Inspection' for functions
that work with lists.
-}
mkPartialInspectionList :: Id Inspection -> NameMeta -> Inspection
mkPartialInspectionList insId nameMeta = mkPartialInspection insId nameMeta "lists"
    & categoryL %~ (<> one Category.list)
    & solutionL .~
        [ "Replace list with 'NonEmpty' from 'Data.List.NonEmpty'"
        , "Use explicit pattern-matching over lists"
        ]

{- | Smart constructor to create partial 'Inspection' for functions
that work with enumerable types.
-}
mkPartialInspectionEnum :: Id Inspection -> Text -> [Text] -> Inspection
mkPartialInspectionEnum insId funName solution = mkPartialInspection insId enumMeta ""
    & descriptionL .~ usage funName "enumerable types"
    & solutionL .~ solution
  where
    enumMeta :: NameMeta
    enumMeta = set moduleNameL "GHC.Enum" $ mkBaseMeta funName

-- | 'Inspection' — partial 'GHC.List.head' @STAN-0001@.
stan0001 :: Inspection
stan0001 = mkPartialInspectionList (Id "STAN-0001") (mkBaseListMeta "head")

-- | 'Inspection' — partial 'GHC.List.tail' @STAN-0002@.
stan0002 :: Inspection
stan0002 = mkPartialInspectionList (Id "STAN-0002") (mkBaseListMeta "tail")

-- | 'Inspection' — partial 'GHC.List.init' @STAN-0003@.
stan0003 :: Inspection
stan0003 = mkPartialInspectionList (Id "STAN-0003") (mkBaseListMeta "init")

-- | 'Inspection' — partial 'GHC.List.last' @STAN-0004@.
stan0004 :: Inspection
stan0004 = mkPartialInspectionList (Id "STAN-0004") (mkBaseListMeta "last")

-- | 'Inspection' — partial 'GHC.List.!!' @STAN-0005@.
stan0005 :: Inspection
stan0005 = mkPartialInspectionList (Id "STAN-0005") (mkBaseListMeta "!!")
    & solutionL .~ []

-- | 'Inspection' — partial 'GHC.List.cycle' @STAN-0006@.
stan0006 :: Inspection
stan0006 = mkPartialInspectionList (Id "STAN-0006") (mkBaseListMeta "cycle")

-- | 'Inspection' — partial 'Data.OldList.genericIndex' @STAN-0007@.
stan0007 :: Inspection
stan0007 = mkPartialInspectionList (Id "STAN-0007") (mkBaseOldListMeta "genericIndex")
    & solutionL .~ []

-- | 'Inspection' — partial 'Data.Maybe.fromJust' @STAN-0008@.
stan0008 :: Inspection
stan0008 = mkPartialInspection (Id "STAN-0008") fromJustNameMeta "'Maybe'"
    & solutionL .~
        [ "Use explicit pattern-matching over Maybe"
        , "Use one of the standard functions: 'maybe', 'fromMaybe'"
        ]
  where
    fromJustNameMeta :: NameMeta
    fromJustNameMeta = set moduleNameL "Data.Maybe" $ mkBaseMeta "fromJust"

-- | 'Inspection' — partial 'Text.Read.read' @STAN-0009@.
stan0009 :: Inspection
stan0009 = mkPartialInspection (Id "STAN-0009") readNameMeta ""
    & descriptionL .~ usage "read" "parsing 'String'"
    & solutionL .~
        [ "Use 'readMaybe' or 'readEither' to handle failed parsing"
        ]
  where
    readNameMeta :: NameMeta
    readNameMeta = set moduleNameL "Text.Read" $ mkBaseMeta "read"

-- | 'Inspection' — partial 'GHC.Enum.succ' @STAN-0010@.
stan0010 :: Inspection
stan0010 = mkPartialInspectionEnum (Id "STAN-0010") "succ"
    [ "Use '(+ 1)' for integral types (but be aware of arithmetic overflow)"
    ]

-- | 'Inspection' — partial 'GHC.Enum.pred' @STAN-0011@.
stan0011 :: Inspection
stan0011 = mkPartialInspectionEnum (Id "STAN-0011") "pred"
    [ "Use '(- 1)' for integral types (but be aware of arithmetic overflow)"
    ]

-- | 'Inspection' — partial 'GHC.Enum.toEnum' @STAN-0012@.
stan0012 :: Inspection
stan0012 = mkPartialInspectionEnum (Id "STAN-0012") "toEnum" []

-- | 'Inspection' — partial 'Data.Foldable.maximum' @STAN-0013@.
stan0013 :: Inspection
stan0013 = mkPartialInspectionPattern
    (Id "STAN-0013") (mkBaseFoldableMeta "maximum") listFunPattern ""

-- | 'Inspection' — partial 'Data.Foldable.minimum' @STAN-0014@.
stan0014 :: Inspection
stan0014 = mkPartialInspectionPattern
    (Id "STAN-0014") (mkBaseFoldableMeta "minimum") listFunPattern ""

orderingFunPattern :: Pattern
orderingFunPattern = (?) |-> listFunPattern

-- | 'Inspection' — partial 'Data.Foldable.maximumBy' @STAN-0015@.
stan0015 :: Inspection
stan0015 = mkPartialInspectionPattern
    (Id "STAN-0015") (mkBaseFoldableMeta "maximumBy") orderingFunPattern ""

-- | 'Inspection' — partial 'Data.Foldable.minimumBy' @STAN-0016@.
stan0016 :: Inspection
stan0016 = mkPartialInspectionPattern
    (Id "STAN-0016") (mkBaseFoldableMeta "minimumBy") orderingFunPattern ""

-- | 'Inspection' — partial 'Data.Foldable.foldl1' @STAN-0017@.
stan0017 :: Inspection
stan0017 = mkPartialInspectionPattern
    (Id "STAN-0017") (mkBaseFoldableMeta "foldl1") orderingFunPattern ""

-- | 'Inspection' — partial 'Data.Foldable.foldl1\'' @STAN-0018@.
stan0018 :: Inspection
stan0018 = mkPartialInspectionList (Id "STAN-0018") (mkBaseListMeta "foldl1'")

-- | 'Inspection' — partial 'Data.Foldable.foldr1' @STAN-0019@.
stan0019 :: Inspection
stan0019 = mkPartialInspectionPattern
    (Id "STAN-0019") (mkBaseFoldableMeta "foldr1") orderingFunPattern ""

-- | 'Inspection' — partial 'GHC.Exts.fromList' @STAN-0020@.
stan0020 :: Inspection
stan0020 = mkPartialInspectionPattern
    (Id "STAN-0020")
    (mkBaseMeta "fromList" & moduleNameL .~ "GHC.Exts")
    (listPattern |-> nonEmptyPattern)
    ""

-- | 'Inspection' — partial 'GHC.Num.fromInteger' @STAN-0021@.
stan0021 :: Inspection
stan0021 = mkPartialInspectionPattern
    (Id "STAN-0021")
    (mkBaseMeta "fromInteger" & moduleNameL .~ "GHC.Num")
    (integerPattern |-> naturalPattern)
    ""
