{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Report settings types.
-}

module Stan.Report.Settings
    ( ReportSettings (..)
    ) where

import Stan.Core.Toggle (ToggleSolution)


{- | Settings for produced report.
-}
data ReportSettings = ReportSettings
    { reportSettingsSolutionVerbosity :: !ToggleSolution
    }
