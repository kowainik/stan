{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

__Observation__ — a vulnerability found in the target project by @Stan@.
-}

module Stan.Observation
    ( Observation (..)

      -- * Pretty print
    , prettyShowObservation
    ) where

import SrcLoc (RealSrcSpan)

import Stan.Core.Id (Id)
import Stan.Inspection (Inspection)


{- | Data type to represent discovered by Stan vulnerabilities.
-}
data Observation = Observation
    { observationId           :: !(Id Observation)
    , observationInspectionId :: !(Id Inspection)
    , observationLoc          :: !RealSrcSpan
    , observationFile         :: !FilePath
    } deriving stock (Show)

-- | Show 'Observation' in a human-friendly format.
prettyShowObservation :: Observation -> Text
prettyShowObservation = show
