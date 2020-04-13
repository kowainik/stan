{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

__Inspection__ — check or test provided by Stan.
-}

module Stan.Inspection
    ( Inspection (..)
    , Severity (..)

      -- * Stan inspections
    , inspections
    , getInspectionById

      -- * Pretty print
    , prettyShowInspection
    , prettyShowSeverity
    , severityColour
    ) where

import Colourista (bold, formatWith, red, yellow)

import Stan.Category (Category, partial)
import Stan.Core.Id (Id (..))


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
    } deriving stock (Show)

-- | Severity level of the inspection.
data Severity
    = Severe
    | NotReallySevere
    deriving stock (Show)


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

{- | List of all inspections.
-}
inspections :: [Inspection]
inspections =
    [ Inspection
        -- TODO: See issue #26: https://github.com/kowainik/stan/issues/26
        { inspectionId = Id "STAN-0001-HEAD"
        , inspectionName = "Partial: base/head"
        , inspectionDescription = "Usage of partial function 'head' for lists"
        , inspectionSolution =
            [ "Replace list with 'NonEmpty' from 'Data.List.NonEmpty'"
            , "Use explicit pattern-matching over lists"
            ]
        , inspectionCategory = one partial
        , inspectionSeverity = Severe
        }
    ]

-- | Get the 'Inspection' by the given known inspection 'Id'.
getInspectionById :: Id Inspection -> Inspection
getInspectionById insId = case find ((==) insId . inspectionId) inspections of
    Just ins -> ins
    -- TODO: how to handle unknown ids better?
    Nothing  -> error "Unknown Inspection ID"
