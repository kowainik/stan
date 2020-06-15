{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Report settings types.
-}

module Stan.Report.Settings
    ( ReportSettings (..)

      -- * Verbosity
    , Verbosity (..)
    , isVerbose
      -- * Toggle
    , ToggleSolution (..)
    , isHidden
    ) where


{- | Settings for produced report.
-}
data ReportSettings = ReportSettings
    { reportSettingsVerbosity         :: !Verbosity
    , reportSettingsSolutionVerbosity :: !ToggleSolution
    }

data Verbosity
    = Verbose
    | NonVerbose
    deriving stock (Show)

isVerbose :: Verbosity -> Bool
isVerbose = \case
     Verbose -> True
     NonVerbose -> False

-- | Boolean for showing/hiding solution information of observations.
data ToggleSolution
    = HideSolution
    | ShowSolution
    deriving stock (Show)

-- | Is the toggle option set to 'HideSolution'?
isHidden :: ToggleSolution -> Bool
isHidden HideSolution = True
isHidden ShowSolution = False
