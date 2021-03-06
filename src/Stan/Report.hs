{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Report and report settings types.
-}

module Stan.Report
    ( generateReport
    ) where

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Stan.Analysis (Analysis)
import Stan.Config (Config)
import Stan.Info (ProjectInfo, StanEnv)
import Stan.Report.Html (stanHtml)


generateReport
    :: Analysis
    -> Config
    -> [Text]  -- ^ Warnings during Trial config selections
    -> StanEnv  -- ^ Environment information
    -> ProjectInfo  -- ^ Project related Information
    -> IO ()
generateReport an c ws env = writeFileLBS "stan.html" . renderHtml .
    stanHtml an c ws env
