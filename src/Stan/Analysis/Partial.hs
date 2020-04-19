{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Static analysis checks for partial functions.
-}

module Stan.Analysis.Partial
    ( analyseForHeadObservations
    ) where

import HieTypes (HieAST (..), HieASTs (..), HieFile (..), Identifier, IdentifierDetails (..),
                 NodeInfo (..), TypeIndex)
import Module (moduleName, moduleNameString, moduleUnitId)
import Name (nameModule, nameOccName)
import OccName (occNameString)
import SrcLoc (RealSrcSpan)

import Stan.Core.Id (Id)
import Stan.Inspection (Inspection, NameMeta (..))
import Stan.Inspection.Partial (stan0001, stan0001Meta)
import Stan.Observation (Observation (..), mkObservationId)

import qualified Data.Map.Strict as Map


-- | Smart constructor for partial 'Observation's.
mkPartialObservation
    :: Id Inspection  -- ^ Corresponding 'Inspection's 'Id'.
    -> HieFile
    -> Int  -- ^ Ordinal number of the 'Observation'.
    -> RealSrcSpan  -- ^ Position.
    -> Observation
mkPartialObservation insId HieFile{..} num srcSpan = Observation
    { observationId = mkObservationId num insId
    , observationInspectionId = insId
    , observationLoc = srcSpan
    , observationFile = hie_hs_file
    , observationModuleName = toText $ moduleNameString $ moduleName hie_module
    , observationFileContent = hie_hs_src
    }

{- | Check for occurrences of the partial @head@ function.
-}
analyseForHeadObservations :: HieFile -> [Observation]
analyseForHeadObservations hie@HieFile{..} =
    zipWith (mkPartialObservation stan0001 hie) [1..] $ findHeads hie_asts
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
    findHeadUsage srcSpan (identifier, _details) = do
        Right name <- Just identifier

        let occName = occNameString $ nameOccName name
        let modul = moduleNameString $ moduleName $ nameModule name
        let package = show @String $ moduleUnitId $ nameModule name
        let NameMeta{..} = stan0001Meta

        guard
             $ occName == toString nameMetaName
            && modul   == toString nameMetaModuleName
            && package == toString nameMetaPackage

        pure srcSpan
