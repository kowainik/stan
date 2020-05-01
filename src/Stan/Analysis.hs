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

import Extensions (ExtensionsError (ModuleParseError, NotCabalModule), ExtensionsResult)
import Extensions.OnOff (OnOffExtension, mergeExtensions)
import Extensions.Parser (parseSourceWithPath)
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
    , analysisUsedExtensions :: !(Set OnOffExtension)
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

extensionsL :: Lens' Analysis (Set OnOffExtension)
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

-- | Collect all ubique used extensions.
addExtensions :: Set OnOffExtension -> State Analysis ()
addExtensions = modify' . over extensionsL . Set.union

-- | Update 'FileInfo' for the given 'FilePath'.
updateFileMap :: FilePath -> FileInfo -> State Analysis ()
updateFileMap fp fi = modify' $ over fileMapL (Map.insert fp fi)

{- | Perform static analysis of given 'HieFile'.
-}
runAnalysis :: Map FilePath ExtensionsResult -> [HieFile] -> Analysis
runAnalysis cabalExtensionsMap = executingState initialAnalysis . analyse cabalExtensionsMap

analyse :: Map FilePath ExtensionsResult -> [HieFile] -> State Analysis ()
analyse _extsMap [] = pass
analyse cabalExtensions (hieFile@HieFile{..}:hieFiles) = do
    -- traceM (hie_hs_file hieFile)
    let fileInfoLoc = countLinesOfCode hieFile
    let fileInfoCabalExtensions = fromMaybe
            (Left $ NotCabalModule hie_hs_file)
            (Map.lookup hie_hs_file cabalExtensions)
    let fileInfoExtensions = bimap
            (ModuleParseError hie_hs_file)
            Set.fromList
            (parseSourceWithPath hie_hs_file hie_hs_src)
    let fileInfoPath = hie_hs_file
    -- merge cabal and module extensions and update overall exts
    let fileMergedExtensions = merge fileInfoCabalExtensions fileInfoExtensions
    let fileInfoObservations = S.concatMap (`analysisByInspection` hieFile) inspections

    incModulesNum
    incLinesOfCode fileInfoLoc
    updateFileMap hie_hs_file FileInfo{..}
    addExtensions fileMergedExtensions
    addObservations fileInfoObservations

    analyse cabalExtensions hieFiles
  where
    merge :: ExtensionsResult -> ExtensionsResult -> Set OnOffExtension
    merge (Left _) (Left _)           = mempty
    merge (Left _) (Right exts)       = exts
    merge (Right exts) (Left _)       = exts
    merge (Right exts1) (Right exts2) = mergeExtensions $ toList exts1 <> toList exts2
