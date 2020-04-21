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
import Module (moduleUnitId)
import Name (nameModule, nameOccName)
import OccName (occNameString)
import Slist (Slist, slist)
import SrcLoc (RealSrcSpan)

import Stan.Core.Id (Id)
import Stan.Core.ModuleName (fromGhcModule)
import Stan.Inspection (Inspection (..), InspectionAnalysis (..))
import Stan.NameMeta (NameMeta (..))
import Stan.Observation (Observations, mkObservation)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Slist as S


{- | Create analysing function for 'Inspection' by pattern-matching
over 'InspectionAnalysis'.
-}
analysisByInspection :: Inspection -> HieFile -> Observations
analysisByInspection Inspection{..} = case inspectionAnalysis of
    FindName nameMeta -> analyseNameMeta inspectionId nameMeta
    Infix             -> const mempty  -- TODO: not yet implemented

{- | Check for occurrences of the specified function given via 'NameMeta'.
-}
analyseNameMeta
  :: Id Inspection
  -> NameMeta
  -> HieFile
  -> Observations
analyseNameMeta insId NameMeta{..} hie@HieFile{..} =
    mkObservation insId hie <$> findHeads hie_asts
  where
    findHeads :: HieASTs TypeIndex -> Slist RealSrcSpan
    findHeads =
        S.concatMap findInAst
        . Map.elems
        . getAsts

    findInAst :: HieAST TypeIndex -> Slist RealSrcSpan
    findInAst Node{..} =
        findInNode nodeSpan nodeInfo <> S.concatMap findInAst nodeChildren

    findInNode :: RealSrcSpan -> NodeInfo TypeIndex -> Slist RealSrcSpan
    findInNode srcSpan NodeInfo{..} = slist
        $ mapMaybe (findHeadUsage srcSpan)
        $ Map.assocs nodeIdentifiers

    findHeadUsage
        :: RealSrcSpan
        -> (Identifier, IdentifierDetails TypeIndex)
        -> Maybe RealSrcSpan
    findHeadUsage srcSpan (identifier, details) = do
        Right name <- Just identifier
        guard $ Set.notMember (IEThing Import) $ identInfo details

        let occName = toText $ occNameString $ nameOccName name
        let moduleName = fromGhcModule $ nameModule name
        let package = show @Text $ moduleUnitId $ nameModule name

        guard
             $ occName    == nameMetaName
            && moduleName == nameMetaModuleName
            && package    == nameMetaPackage

        pure srcSpan
