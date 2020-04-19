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

import Stan.Inspection (stan0001)
import Stan.Observation (Observation (..), mkObservationId)

import qualified Data.Map.Strict as Map


{- | Check for occurrences of the partial @head@ function.
-}
analyseForHeadObservations :: HieFile -> [Observation]
analyseForHeadObservations HieFile{..} =
    zipWith mkPartialObservation [1..] $ findHeads hie_asts
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

        guard
             $ occName == "head"
            && modul   == "GHC.List"
            && package == "base"

        pure srcSpan

    mkPartialObservation :: Int -> RealSrcSpan -> Observation
    mkPartialObservation num srcSpan = Observation
        { observationId = mkObservationId num stan0001
        , observationInspectionId = stan0001
        , observationLoc = srcSpan
        , observationFile = hie_hs_file
        , observationModuleName = toText $ moduleNameString $ moduleName hie_module
        , observationFileContent = hie_hs_src
        }
