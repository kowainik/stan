{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Report and report settings types.
-}

module Stan.Report
    ( generateReport
    ) where

import Html (renderByteString)

import Stan.Analysis (Analysis)
import Stan.Config (Config)
import Stan.Info (StanEnv)
import Stan.Report.Html (stanHtml)


generateReport
    :: Analysis
    -> Config
    -> [Text]  -- ^ Warnings during Trial config selections
    -> StanEnv  -- ^ Environment information
    -> IO ()
generateReport an c ws = writeFileLBS "stan.html" . renderByteString .
    stanHtml an c ws
