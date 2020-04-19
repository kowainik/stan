{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

-}

module Stan.Analysis.Analyser
    ( Analyser (..)
    ) where

import HieTypes (HieFile)

import Stan.Core.Id (Id)
import Stan.Inspection (Inspection)
import Stan.Observation (Observation)


{- | Contains analyser function to run on 'HieFile's.
-}
data Analyser = Analyser
    { analyserInspectionId :: !(Id Inspection)
    , analyserFunction     :: !(HieFile -> [Observation])
    }
