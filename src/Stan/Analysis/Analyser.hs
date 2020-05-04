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

import HieTypes (ContextInfo (..), HieAST (..), HieASTs (..), HieFile (..), IEType (..), Identifier,
                 IdentifierDetails (..), NodeInfo (..), TypeIndex)
import Slist (Slist, slist)
import SrcLoc (RealSrcSpan)

import Stan.Core.Id (Id)
import Stan.Hie.Match (hieMatchPattern)
import Stan.Inspection (Inspection (..), InspectionAnalysis (..))
import Stan.NameMeta (NameMeta (..), compareNames)
import Stan.Observation (Observations, mkObservation)
import Stan.Pattern (Pattern (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
  -> Pattern
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
    findUsage typeIxs srcSpan (identifier, details) = do
        Right name <- Just identifier
        guard
            -- not in the imports
            $ Set.notMember (IEThing Import) (identInfo details)
            -- exact name/module/package
            && compareNames nameMeta name
            -- compatible with the given pattern
            && any (hieMatchPattern hie_types pat) typeIxs

        pure srcSpan
