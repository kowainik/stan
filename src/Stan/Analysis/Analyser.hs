{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Analysing function with an 'Id' of the corresponding 'Inspection'.
-}

module Stan.Analysis.Analyser
    ( Analyser (..)
    , mkNameMetaAnalyser
    ) where

import HieTypes (ContextInfo (..), HieAST (..), HieASTs (..), HieFile (..), IEType (..), Identifier,
                 IdentifierDetails (..), NodeInfo (..), TypeIndex)
import Module (moduleUnitId)
import Name (nameModule, nameOccName)
import OccName (occNameString)
import SrcLoc (RealSrcSpan)

import Stan.Core.Id (Id)
import Stan.Core.ModuleName (fromGhcModule)
import Stan.Inspection (Inspection)
import Stan.NameMeta (NameMeta (..))
import Stan.Observation (Observation, mkObservation)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


{- | Contains analyser function to run on 'HieFile's.
-}
data Analyser = Analyser
    { analyserInspectionId :: !(Id Inspection)
    , analyserFunction     :: !(HieFile -> [Observation])
    }

-- | Smart constructor for 'Analyser' creation of partial functions.
mkNameMetaAnalyser :: Id Inspection -> NameMeta -> Analyser
mkNameMetaAnalyser insId nameMeta = Analyser
    { analyserInspectionId = insId
    , analyserFunction = analyseNameMeta insId nameMeta
    }

{- | Check for occurrences of the specified function given via 'NameMeta'.
-}
analyseNameMeta
  :: Id Inspection
  -> NameMeta
  -> HieFile
  -> [Observation]
analyseNameMeta insId NameMeta{..} hie@HieFile{..} =
    map (mkObservation insId hie) $ findHeads hie_asts
  where
    findHeads :: HieASTs TypeIndex -> [RealSrcSpan]
    findHeads =
        concatMap findInAst
        . Map.elems
        . getAsts

    findInAst :: HieAST TypeIndex -> [RealSrcSpan]
    findInAst Node{..} =
        findInNode nodeSpan nodeInfo ++ concatMap findInAst nodeChildren

    findInNode :: RealSrcSpan -> NodeInfo TypeIndex -> [RealSrcSpan]
    findInNode srcSpan NodeInfo{..} =
        mapMaybe (findHeadUsage srcSpan)
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
