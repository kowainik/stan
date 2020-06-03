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
import Stan.Report.Html (stanHtml)


generateReport :: Analysis -> IO ()
generateReport = writeFileLBS "stan.html" . renderByteString . stanHtml
