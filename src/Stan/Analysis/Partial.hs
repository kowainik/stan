{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Static analysis checks for partial functions.
-}

module Stan.Analysis.Partial
    ( stan0001Analyser
    , stan0002Analyser
    , stan0003Analyser
    , stan0004Analyser

      -- * All partial 'Analyser's
    , partialAnalysers
    ) where

import HieTypes (HieAST (..), HieASTs (..), HieFile (..), Identifier, IdentifierDetails (..),
                 NodeInfo (..), TypeIndex)
import Module (moduleName, moduleNameString, moduleUnitId)
import Name (nameModule, nameOccName)
import OccName (occNameString)
import SrcLoc (RealSrcSpan)

import Stan.Analysis.Analyser (Analyser (..))
import Stan.Core.Id (Id)
import Stan.Inspection (Inspection, NameMeta (..))
import Stan.Inspection.Partial (stan0001, stan0001Meta, stan0002, stan0002Meta, stan0003,
                                stan0003Meta, stan0004, stan0004Meta)
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
analyseForPartialObservations
  :: Id Inspection
  -> NameMeta
  -> HieFile
  -> [Observation]
analyseForPartialObservations insId NameMeta{..} hie@HieFile{..} =
    zipWith (mkPartialObservation insId hie) [1..] $ findHeads hie_asts
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
             $ occName == toString nameMetaName
            && modul   == toString nameMetaModuleName
            && package == toString nameMetaPackage

        pure srcSpan

-- | Smart constructor for 'Analyser' creation of partial functions.
mkPartialAnalyser :: Id Inspection -> NameMeta -> Analyser
mkPartialAnalyser insId nameMeta = Analyser
    { analyserInspectionId = insId
    , analyserFunction = analyseForPartialObservations insId nameMeta
    }

{- | Check for occurrences of the partial 'GHC.List.head' function.
@STAN-0001@.
-}
stan0001Analyser :: Analyser
stan0001Analyser = mkPartialAnalyser stan0001 stan0001Meta

{- | Check for occurrences of the partial 'GHC.List.tail' function.
@STAN-0002@.
-}
stan0002Analyser :: Analyser
stan0002Analyser = mkPartialAnalyser stan0002 stan0002Meta

{- | Check for occurrences of the partial 'GHC.List.init' function.
@STAN-0003@.
-}
stan0003Analyser :: Analyser
stan0003Analyser = mkPartialAnalyser stan0003 stan0003Meta

{- | Check for occurrences of the partial 'GHC.List.last' function.
@STAN-0004@.
-}
stan0004Analyser :: Analyser
stan0004Analyser = mkPartialAnalyser stan0004 stan0004Meta

-- | List of all partial 'Analyser's.
partialAnalysers :: [Analyser]
partialAnalysers =
    [ stan0001Analyser
    , stan0002Analyser
    , stan0003Analyser
    , stan0004Analyser
    ]
