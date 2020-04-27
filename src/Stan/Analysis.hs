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

import Extensions (ExtensionsResult)
import Extensions.OnOff (OnOffExtension)
import HieTypes (HieFile (..))
import Relude.Extra.Lens (Lens', lens, over)

import Stan.Analysis.Analyser (analysisByInspection)
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

initialAnalysis :: Analysis
initialAnalysis = Analysis
    { analysisModulesNum     = 0
    , analysisLinesOfCode    = 0
    , analysisUsedExtensions = mempty
    , analysisObservations   = mempty
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

addExtensions :: ExtensionsResult -> State Analysis ()
addExtensions = \case
    Right setExtensions -> modify' $ over extensionsL (Set.union setExtensions)
    Left _err -> pass

{- | Perform static analysis of given 'HieFile'.
-}
runAnalysis :: Map FilePath ExtensionsResult -> [HieFile] -> Analysis
runAnalysis extensionsMap = executingState initialAnalysis . analyse extensionsMap

analyse :: Map FilePath ExtensionsResult -> [HieFile] -> State Analysis ()
analyse _extsMap [] = pass
analyse extsMap (hieFile:hieFiles) = do
    -- traceM (hie_hs_file hieFile)
    incModulesNum
    incLinesOfCode $ countLinesOfCode hieFile
    addObservations $ S.concatMap (`analysisByInspection` hieFile) inspections
    -- Add found extensions
    whenJust (Map.lookup (hie_hs_file hieFile) extsMap) addExtensions

    analyse extsMap hieFiles
