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

import HieTypes (HieFile (..))
import Relude.Extra.Lens (Lens', lens, over)

import Stan.Hie (countLinesOfCode)


{- | This data type stores all information collected during static analysis.
-}
data Analysis = Analysis
    { analysisModulesNum     :: !Int
    , analysisLinesOfCode    :: !Int
    , analysisUsedExtensions :: !Int
    , analysisObservations   :: !()  -- TODO: use Observation type later
    } deriving stock (Show)

modulesNumL :: Lens' Analysis Int
modulesNumL = lens
    analysisModulesNum
    (\analysis new -> analysis { analysisModulesNum = new })

linesOfCodeL :: Lens' Analysis Int
linesOfCodeL = lens
    analysisLinesOfCode
    (\analysis new -> analysis { analysisLinesOfCode = new })


initialAnalysis :: Analysis
initialAnalysis = Analysis
    { analysisModulesNum     = 0
    , analysisLinesOfCode    = 0
    , analysisUsedExtensions = 0
    , analysisObservations   = ()
    }

incModulesNum :: State Analysis ()
incModulesNum = modify' $ over modulesNumL (+ 1)

{- | Increase the total loc ('analysisLinesOfCode') by the given number of
analised lines of code.
-}
incLinesOfCode :: Int -> State Analysis ()
incLinesOfCode num = modify' $ over linesOfCodeL (+ num)

{- | Perform static analysis of given 'HieFile'.
-}
runAnalysis :: [HieFile] -> Analysis
runAnalysis = executingState initialAnalysis . analyse

analyse :: [HieFile] -> State Analysis ()
analyse [] = pass
analyse (hieFile:hieFiles) = do
    incModulesNum
    -- traceM (hie_hs_file hieFile)
    incLinesOfCode $ countLinesOfCode hieFile
    analyse hieFiles
