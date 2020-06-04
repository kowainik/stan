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

import Extensions (ExtensionsError (..), OnOffExtension, ParsedExtensions (..),
                   SafeHaskellExtension, parseSourceWithPath)
import HieTypes (HieFile (..))
import Relude.Extra.Lens (Lens', lens, over)

import Stan.Analysis.Analyser (analysisByInspection)
import Stan.Cabal (mergeParsedExtensions)
import Stan.Core.Id (Id)
import Stan.Core.ModuleName (fromGhcModule)
import Stan.FileInfo (FileInfo (..), FileMap)
import Stan.Hie (countLinesOfCode)
import Stan.Inspection (Inspection)
import Stan.Inspection.All (lookupInspectionById)
import Stan.Observation (Observation (..), Observations)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Slist as S


{- | This data type stores all information collected during static analysis.
-}
data Analysis = Analysis
    { analysisModulesNum          :: !Int
    , analysisLinesOfCode         :: !Int
    , analysisUsedExtensions      :: !(Set OnOffExtension, Set SafeHaskellExtension)
    , analysisInspections         :: !(HashSet (Id Inspection))
    , analysisObservations        :: !Observations
    , analysisIgnoredObservations :: !Observations
    , analysisFileMap             :: !FileMap
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

inspectionsL :: Lens' Analysis (HashSet (Id Inspection))
inspectionsL = lens
    analysisInspections
    (\analysis new -> analysis { analysisInspections = new })

observationsL :: Lens' Analysis Observations
observationsL = lens
    analysisObservations
    (\analysis new -> analysis { analysisObservations = new })

ignoredObservationsL :: Lens' Analysis Observations
ignoredObservationsL = lens
    analysisIgnoredObservations
    (\analysis new -> analysis { analysisIgnoredObservations = new })

fileMapL :: Lens' Analysis FileMap
fileMapL = lens
    analysisFileMap
    (\analysis new -> analysis { analysisFileMap = new })

initialAnalysis :: Analysis
initialAnalysis = Analysis
    { analysisModulesNum          = 0
    , analysisLinesOfCode         = 0
    , analysisUsedExtensions      = mempty
    , analysisInspections         = mempty
    , analysisObservations        = mempty
    , analysisIgnoredObservations = mempty
    , analysisFileMap             = mempty
    }

incModulesNum :: State Analysis ()
incModulesNum = modify' $ over modulesNumL (+ 1)

{- | Increase the total loc ('analysisLinesOfCode') by the given number of
analised lines of code.
-}
incLinesOfCode :: Int -> State Analysis ()
incLinesOfCode num = modify' $ over linesOfCodeL (+ num)

-- | Add set of 'Inspection' 'Id's to the existing set.
addInspections :: HashSet (Id Inspection) -> State Analysis ()
addInspections ins = modify' $ over inspectionsL (ins <>)

-- | Add list of 'Observation's to the beginning of the existing list
addObservations :: Observations -> State Analysis ()
addObservations observations = modify' $ over observationsL (observations <>)

-- | Add list of 'Observation's to the beginning of the existing list
addIgnoredObservations :: Observations -> State Analysis ()
addIgnoredObservations obs = modify' $ over ignoredObservationsL (obs <>)

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
runAnalysis
    :: Map FilePath (Either ExtensionsError ParsedExtensions)
    -> HashMap FilePath (HashSet (Id Inspection))
    -> [Id Observation]  -- ^ List of to-be-ignored Observations.
    -> [HieFile]
    -> Analysis
runAnalysis cabalExtensionsMap checksMap obs = executingState initialAnalysis .
    analyse cabalExtensionsMap checksMap obs

analyse
    :: Map FilePath (Either ExtensionsError ParsedExtensions)
    -> HashMap FilePath (HashSet (Id Inspection))
    -> [Id Observation]  -- ^ List of to-be-ignored Observations.
    -> [HieFile]
    -> State Analysis ()
analyse _extsMap _checksMap _observations [] = pass
analyse cabalExtensions checksMap observations (hieFile@HieFile{..}:hieFiles) = do
    whenJust (HM.lookup hie_hs_file checksMap)
        (analyseHieFile hieFile cabalExtensions observations)
    analyse cabalExtensions checksMap observations hieFiles

analyseHieFile
    :: HieFile
    -> Map FilePath (Either ExtensionsError ParsedExtensions)
    -> [Id Observation]  -- ^ List of to-be-ignored Observations.
    -> HashSet (Id Inspection)
    -> State Analysis ()
analyseHieFile hieFile@HieFile{..} cabalExts obs insIds = do
    -- traceM (hie_hs_file hieFile)
    let fileInfoLoc = countLinesOfCode hieFile
    let fileInfoCabalExtensions = fromMaybe
            (Left $ NotCabalModule hie_hs_file)
            (Map.lookup hie_hs_file cabalExts)
    let fileInfoExtensions = first (ModuleParseError hie_hs_file) $
            parseSourceWithPath hie_hs_file hie_hs_src
    let fileInfoPath = hie_hs_file
    let fileInfoModuleName = fromGhcModule hie_module
    -- merge cabal and module extensions and update overall exts
    let fileInfoMergedExtensions = mergeParsedExtensions fileInfoCabalExtensions fileInfoExtensions
    -- get list of inspections for the file
    let ins = mapMaybe lookupInspectionById (toList insIds)
    let allObservations = S.concatMap (`analysisByInspection` hieFile) ins
    let (ignoredObs, fileInfoObservations) = S.partition ((`elem` obs) . observationId) allObservations

    incModulesNum
    incLinesOfCode fileInfoLoc
    updateFileMap hie_hs_file FileInfo{..}
    whenRight_ fileInfoExtensions addExtensions
    whenRight_ fileInfoCabalExtensions addExtensions
    addInspections insIds
    addObservations fileInfoObservations
    addIgnoredObservations ignoredObs
