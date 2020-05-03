{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Static analysis of all HIE files.
-}

module Stan.Analysis
    ( Analysis (..)
    , runAnalysis
    ) where

import Extensions (ExtensionsError (..), ExtensionsResult, OnOffExtension, ParsedExtensions (..),
                   SafeHaskellExtension, mergeAnyExtensions, parseSourceWithPath)
import HieTypes (HieFile (..))
import Relude.Extra.Lens (Lens', lens, over)

import Stan.Analysis.Analyser (analysisByInspection)
import Stan.FileInfo (FileInfo (..), FileMap)
import Stan.Hie (countLinesOfCode)
import Stan.Inspection.All (inspections)
import Stan.Observation (Observations)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Slist as S


{- | This data type stores all information collected during static analysis.
-}
data Analysis = Analysis
    { analysisModulesNum     :: !Int
    , analysisLinesOfCode    :: !Int
    , analysisUsedExtensions :: !(Set OnOffExtension, Set SafeHaskellExtension)
    , analysisObservations   :: !Observations
    , analysisFileMap        :: !FileMap
    } deriving stock (Show)

modulesNumL :: Lens' Analysis Int
modulesNumL = lens
    analysisModulesNum
    (\analysis new -> analysis { analysisModulesNum = new })

linesOfCodeL :: Lens' Analysis Int
linesOfCodeL = lens
    analysisLinesOfCode
    (\analysis new -> analysis { analysisLinesOfCode = new })

extensionsL :: Lens' Analysis (Set OnOffExtension, Set SafeHaskellExtension)
extensionsL = lens
    analysisUsedExtensions
    (\analysis new -> analysis { analysisUsedExtensions = new })

observationsL :: Lens' Analysis Observations
observationsL = lens
    analysisObservations
    (\analysis new -> analysis { analysisObservations = new })

fileMapL :: Lens' Analysis FileMap
fileMapL = lens
    analysisFileMap
    (\analysis new -> analysis { analysisFileMap = new })

initialAnalysis :: Analysis
initialAnalysis = Analysis
    { analysisModulesNum     = 0
    , analysisLinesOfCode    = 0
    , analysisUsedExtensions = mempty
    , analysisObservations   = mempty
    , analysisFileMap        = mempty
    }

incModulesNum :: State Analysis ()
incModulesNum = modify' $ over modulesNumL (+ 1)

{- | Increase the total loc ('analysisLinesOfCode') by the given number of
analised lines of code.
-}
incLinesOfCode :: Int -> State Analysis ()
incLinesOfCode num = modify' $ over linesOfCodeL (+ num)

-- | Add list of 'Observation's to the beginning of the existing list
addObservations :: Observations -> State Analysis ()
addObservations observations = modify' $ over observationsL (observations <>)

-- | Collect all unique used extensions.
addExtensions :: ParsedExtensions -> State Analysis ()
addExtensions ParsedExtensions{..} = modify' $ over extensionsL
    (\(setExts, setSafeExts) ->
        ( Set.union (Set.fromList parsedExtensionsAll) setExts
        , maybe setSafeExts (`Set.insert` setSafeExts) parsedExtensionsSafe
        )
    )

-- | Update 'FileInfo' for the given 'FilePath'.
updateFileMap :: FilePath -> FileInfo -> State Analysis ()
updateFileMap fp fi = modify' $ over fileMapL (Map.insert fp fi)

{- | Perform static analysis of given 'HieFile'.
-}
runAnalysis :: Map FilePath (Either ExtensionsError ParsedExtensions) -> [HieFile] -> Analysis
runAnalysis cabalExtensionsMap = executingState initialAnalysis . analyse cabalExtensionsMap

analyse
    :: Map FilePath (Either ExtensionsError ParsedExtensions)
    -> [HieFile]
    -> State Analysis ()
analyse _extsMap [] = pass
analyse cabalExtensions (hieFile@HieFile{..}:hieFiles) = do
    -- traceM (hie_hs_file hieFile)
    let fileInfoLoc = countLinesOfCode hieFile
    let fileInfoCabalExtensions = fromMaybe
            (Left $ NotCabalModule hie_hs_file)
            (Map.lookup hie_hs_file cabalExtensions)
    let fileInfoExtensions = first (ModuleParseError hie_hs_file) $
            parseSourceWithPath hie_hs_file hie_hs_src
    let fileInfoPath = hie_hs_file
    -- merge cabal and module extensions and update overall exts
    let fileInfoMergedExtensions = merge fileInfoCabalExtensions fileInfoExtensions
    let fileInfoObservations = S.concatMap (`analysisByInspection` hieFile) inspections

    incModulesNum
    incLinesOfCode fileInfoLoc
    updateFileMap hie_hs_file FileInfo{..}
    whenRight_ fileInfoExtensions addExtensions
    whenRight_ fileInfoCabalExtensions addExtensions
    addObservations fileInfoObservations

    analyse cabalExtensions hieFiles
  where
    merge
        :: Either ExtensionsError ParsedExtensions
        -> Either ExtensionsError ParsedExtensions
        -> ExtensionsResult
    merge (Left err) _                = Left err
    merge _ (Left err)                = Left err
    merge (Right exts1) (Right exts2) = mergeAnyExtensions exts1 exts2
