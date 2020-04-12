{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

__Inspection__ — check or test provided by Stan.
-}

module Stan.Inspection
    ( Inspection (..)
    , Category (..)
    , Severity (..)

    , inspections

      -- * Pretty print
    , prettyShowInspection
    ) where

import Stan.Core.Id (Id (..))


{- | Data type that represents a check/test, or how we call it
__inspection__ that is provided by the Stan tool.
-}
data Inspection = Inspection
    { inspectionId          :: !(Id Inspection)
    , inspectionName        :: !Text
    , inspectionDescription :: !Text
    , inspectionSolution    :: !Text
    , inspectionCategory    :: !(NonEmpty Category)
    , inspectionSeverity    :: !Severity
    } deriving stock (Show)

-- | A type of the inspection.
newtype Category = Category
    { unCategory :: Text
    } deriving newtype (Show)

-- | Severity level of the inspection.
data Severity
    = Severe
    | NotReallySevere
    deriving stock (Show)


-- | Show 'Inspection' in a human-friendly format.
prettyShowInspection :: Inspection -> Text
prettyShowInspection = show

{- | List of all inspections.
-}
inspections :: [Inspection]
inspections =
    [ Inspection
        { inspectionId = Id "HEAD"  -- TODO: we need to come up with naming scheme for inspection ids
        , inspectionName = "Partial 'head'"  -- TODO: does it make sense?
        , inspectionDescription = "Usage of partial function 'head' for lists"
        , inspectionSolution = unlines
            [ "* Replace list with 'NonEmpty' from 'Data.List.NonEmpty'"
            , "* Use explicit pattern-matching over lists"
            ]
        , inspectionCategory =
            one $ Category "Partial"  -- TODO: should we convert this to values to avoid typos?
        , inspectionSeverity = Severe
        }
    ]
