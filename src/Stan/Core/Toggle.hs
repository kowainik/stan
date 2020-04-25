{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types to show/hide different parts of inspections and observations.
-}

module Stan.Core.Toggle
    ( ToggleSolution (..)

    , isHidden
    ) where


-- | Boolean for showing/hiding solution information of observations.
data ToggleSolution
    = HideSolution
    | ShowSolution
    deriving stock (Show)

-- | Is the toggle option set to 'HideSolution'?
isHidden :: ToggleSolution -> Bool
isHidden HideSolution = True
isHidden ShowSolution = False
