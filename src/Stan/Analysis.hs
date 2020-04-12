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
import Relude.Extra.Lens (Lens', lens, over)

import Stan.Hie (countLinesOfCode)


{- | This data type stores all information collected during static analysis.
-}
data Analysis = Analysis
    { analysisModuleCount    :: !Int
    , analysisLinesOfCode    :: !Int
    , analysisUsedExtensions :: !Int
    , analysisObservations   :: !()  -- TODO: use Observation type later
    } deriving stock (Show)

moduleCountL :: Lens' Analysis Int
moduleCountL = lens
    analysisModuleCount
    (\analysis new -> analysis { analysisModuleCount = new })

linesOfCodeL :: Lens' Analysis Int
linesOfCodeL = lens
    analysisLinesOfCode
    (\analysis new -> analysis { analysisLinesOfCode = new })


initialAnalysis :: Analysis
initialAnalysis = Analysis
    { analysisModuleCount    = 0
    , analysisLinesOfCode    = 0
    , analysisUsedExtensions = 0
    , analysisObservations   = ()
    }

incModuleCount :: State Analysis ()
incModuleCount = modify' $ over moduleCountL (+ 1)

{- | Increase the total loc ('analysisLinesOfCode') by the given number of
analised lines of code.
-}
incLinesCount :: Int -> State Analysis ()
incLinesCount num = modify' $ over linesOfCodeL (+ num)

{- | Perform static analysis of given 'HieFile'.
-}
runAnalysis :: [HieFile] -> Analysis
runAnalysis = executingState initialAnalysis . analyse

analyse :: [HieFile] -> State Analysis ()
analyse [] = pass
analyse (hieFile:hieFiles) = do
    incModuleCount
    incLinesCount $ countLinesOfCode hieFile
    analyse hieFiles
