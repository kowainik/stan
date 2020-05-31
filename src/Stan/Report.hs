{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Report and report settings types.
-}

module Stan.Report
    ( ReportSettings (..)

    , generateReport
    ) where

import Stan.Core.Toggle (ToggleSolution)


{- | Settings for produced report.
-}
data ReportSettings = ReportSettings
    { reportSettingsSolutionVerbosity :: !ToggleSolution
    }

generateReport :: Text -> IO ()
generateReport = writeFileText "stan.html"
    . unlines . ("<pre>":) . map (<> "<br/>") . lines
