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

      -- ** Inspections by ID
    , stan0001
    , stan0001Inspection

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

{- | List of all inspections.
-}
inspections :: [Inspection]
inspections =
    [ stan0001Inspection
    ]

-- | 'Id' fo the partial 'head' 'Inspection' — @STAN-0001@.
stan0001 :: Id Inspection
stan0001 = Id "STAN-0001"

-- | Corresponding 'Inspection' for 'stan0001' — partial 'head' @STAN-0001@.
stan0001Inspection :: Inspection
stan0001Inspection =Inspection
    { inspectionId = stan0001
    , inspectionName = "Partial: base/head"
    , inspectionDescription = "Usage of partial function 'head' for lists"
    , inspectionSolution =
        [ "Replace list with 'NonEmpty' from 'Data.List.NonEmpty'"
        , "Use explicit pattern-matching over lists"
        ]
    , inspectionCategory = one partial
    , inspectionSeverity = Severe
    }

-- | Get the 'Inspection' by the given known inspection 'Id'.
getInspectionById :: Id Inspection -> Inspection
getInspectionById insId = case find ((==) insId . inspectionId) inspections of
    Just ins -> ins
    -- TODO: how to handle unknown ids better?
    Nothing  -> error "Unknown Inspection ID"
