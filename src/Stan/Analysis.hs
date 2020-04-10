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

import HieTypes (HieFile)


{- | This data type stores all information collected during static analysis.
-}
data Analysis = Analysis
    { analysisModuleCount    :: !Int
    , analysisLinesOfCode    :: !Int
    , analysisUsedExtensions :: !Int
    , analysisObservations   :: !()  -- TODO: use Observation type later
    }

initialAnalysis :: Analysis
initialAnalysis = Analysis
    { analysisModuleCount    = 0
    , analysisLinesOfCode    = 0
    , analysisUsedExtensions = 0
    , analysisObservations   = ()
    }

-- QUESTION: is it better to create a newtype? Also, help with the name...
type AnalyseM a = State Analysis a

incModuleCount :: AnalyseM ()
incModuleCount = modify' $
    \analysis -> analysis { analysisModuleCount = analysisModuleCount analysis + 1 }
    -- we probably will use lenses for this kind of stuff in the future...

{- | Perform static analysis of given 'HieFile'.
-}
runAnalysis :: [HieFile] -> Analysis
runAnalysis = executingState initialAnalysis . analyse

analyse :: [HieFile] -> AnalyseM ()
analyse [] = pass
analyse (_hieFile:hieFiles) = do
    incModuleCount
    analyse hieFiles
