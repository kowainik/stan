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

import Stan.Core.Id (Id (..))
import Stan.Observation (Observation (..))

import qualified Data.Map.Strict as Map


{- | Check for occurrences of the partial @head@ function.
-}
analyseForHeadObservations :: HieFile -> [Observation]
analyseForHeadObservations HieFile{..} =
    map mkPartialObservation $ findHeads hie_asts
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

        guard $ and
            [ occName == "head"
            , modul   == "GHC.List"
            , package == "base"
            ]

        pure srcSpan

    mkPartialObservation :: RealSrcSpan -> Observation
    mkPartialObservation srcSpan = Observation
        { observationId = Id ""  -- TODO: how to create observation id?
        , observationInspectionId = Id "HEAD"
        , observationLoc = srcSpan
        , observationFile = hie_hs_file
        }
