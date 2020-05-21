{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Analysing functions by 'InspectionAnalysis' for the corresponding
'Inspection'.
-}

module Stan.Analysis.Analyser
    ( analysisByInspection
    ) where

import HieTypes (HieAST (..), HieASTs (..), HieFile (..), Identifier, IdentifierDetails (..),
                 NodeInfo (..), TypeIndex)
import Slist (Slist, slist)
import SrcLoc (RealSrcSpan)

import Stan.Core.Id (Id)
import Stan.Hie.MatchType (hieMatchPatternType)
import Stan.Inspection (Inspection (..), InspectionAnalysis (..))
import Stan.NameMeta (NameMeta, hieMatchNameMeta)
import Stan.Observation (Observations, mkObservation)
import Stan.Pattern.Type (PatternType (..))

import qualified Data.Map.Strict as Map
import qualified Slist as S


{- | Create analysing function for 'Inspection' by pattern-matching
over 'InspectionAnalysis'.
-}
analysisByInspection :: Inspection -> HieFile -> Observations
analysisByInspection Inspection{..} = case inspectionAnalysis of
    FindName nameMeta pat -> analyseNameMeta inspectionId nameMeta pat
    Infix                 -> const mempty  -- TODO: not yet implemented

{- | Check for occurrences of the specified function given via 'NameMeta'.
-}
analyseNameMeta
  :: Id Inspection
  -> NameMeta
  -> PatternType
  -> HieFile
  -> Observations
analyseNameMeta insId nameMeta pat hie@HieFile{..} =
    mkObservation insId hie <$> findSpans hie_asts
  where
    findSpans :: HieASTs TypeIndex -> Slist RealSrcSpan
    findSpans =
        S.concatMap findInAst
        . Map.elems
        . getAsts

    findInAst :: HieAST TypeIndex -> Slist RealSrcSpan
    findInAst Node{..} =
        findInNode nodeSpan nodeInfo <> S.concatMap findInAst nodeChildren

    findInNode :: RealSrcSpan -> NodeInfo TypeIndex -> Slist RealSrcSpan
    findInNode srcSpan NodeInfo{..} = slist
        $ mapMaybe (findUsage nodeType srcSpan)
        $ Map.assocs nodeIdentifiers

    findUsage
        :: [TypeIndex]
        -> RealSrcSpan
        -> (Identifier, IdentifierDetails TypeIndex)
        -> Maybe RealSrcSpan
    findUsage typeIxs srcSpan hieId = do
        guard
            -- matches with the given nameMeta
            $ hieMatchNameMeta nameMeta hieId
            -- compatible with the given pattern
            && any (hieMatchPatternType hie_types pat) typeIxs

        pure srcSpan
