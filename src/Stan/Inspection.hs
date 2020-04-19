{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

__Inspection__ — check or test provided by Stan.
-}

module Stan.Inspection
    ( Inspection (..)
    , Severity (..)

      -- * Pretty print
    , prettyShowInspection
    , prettyShowSeverity
    , severityColour
    ) where

import Colourista (bold, formatWith, red, yellow)

import Stan.Category (Category)
import Stan.Core.Id (Id)


{- | Data type that represents a check/test, or how we call it
__inspection__ that is provided by the Stan tool.
-}
data Inspection = Inspection
    { inspectionId          :: !(Id Inspection)
    , inspectionName        :: !Text
    , inspectionDescription :: !Text
    , inspectionSolution    :: ![Text]
    , inspectionCategory    :: !(NonEmpty Category)
    , inspectionSeverity    :: !Severity
    } deriving stock (Show, Eq)

-- | Severity level of the inspection.
data Severity
    = Severe
    | NotReallySevere
    deriving stock (Show, Eq)


-- | Show 'Inspection' in a human-friendly format.
prettyShowInspection :: Inspection -> Text
prettyShowInspection = show

-- | Get the colour of the severity level.
severityColour :: Severity -> Text
severityColour = \case
    Severe -> red
    NotReallySevere -> yellow

-- | Show 'Severity' in a human-friendly format.
prettyShowSeverity :: Severity -> Text
prettyShowSeverity s = formatWith [severityColour s, bold] $ show s
