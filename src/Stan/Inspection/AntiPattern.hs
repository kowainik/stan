{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains all 'Inspection's for known anti-patterns.

The __anti-pattern__ inspections are in ranges:

* @STAN-0201 .. STAN-0300@

-}

module Stan.Inspection.AntiPattern
    ( -- * Anti-pattern inspections
      -- *** Anti-pattern @[0 .. length xs]@
      stan0201
      -- *** Anti-pattern 'foldl'
    , stan0202

      -- * All inspections
    , antiPatternInspectionsMap
    ) where

import Relude.Extra.Lens ((%~), (.~))
import Relude.Extra.Tuple (fmapToFst)

import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..), InspectionAnalysis (..), InspectionsMap, categoryL,
                        descriptionL, severityL, solutionL)
import Stan.NameMeta (mkBaseFoldableMeta)
import Stan.Pattern.Ast (PatternAst (..), app, range)
import Stan.Pattern.Edsl (PatternBool (..))
import Stan.Severity (Severity (Error, PotentialBug))

import qualified Data.List.NonEmpty as NE
import qualified Stan.Category as Category


-- | All anti-pattern 'Inspection's map from 'Id's.
antiPatternInspectionsMap :: InspectionsMap
antiPatternInspectionsMap = fromList $ fmapToFst inspectionId
    [ stan0201
    , stan0202
    ]

-- | Smart constructor to create anti-pattern 'Inspection'.
mkAntiPatternInspection :: Id Inspection -> Text -> InspectionAnalysis -> Inspection
mkAntiPatternInspection insId name inspectionAnalysis = Inspection
    { inspectionId = insId
    , inspectionName = "Anti-pattern: " <> name
    , inspectionDescription = ""
    , inspectionSolution = []
    , inspectionCategory = Category.antiPattern :| []
    , inspectionSeverity = PotentialBug
    , ..
    }

-- | 'Inspection' — @[0 .. length xs]@ @STAN-0201@.
stan0201 :: Inspection
stan0201 = mkAntiPatternInspection (Id "STAN-0201") "[0 .. length xs]" (FindAst lenPatAst)
    & descriptionL .~ "Creating a list with wrong number of indices"
    & solutionL .~
        [ "Replace '[0 .. length xs]' with '[0 .. length xs - 1]'"
        , "Use 'zip [0 ..] xs` to work with list of pairs: index and element"
        ]

-- | 'Inspection' — 'foldl' @STAN-0201@.
stan0202 :: Inspection
stan0202 = mkAntiPatternInspection (Id "STAN-0202") "foldl"
    (FindName (mkBaseFoldableMeta "foldl") (?))
    & descriptionL .~ "Usage of space-leaking function 'foldl'"
    & solutionL .~
        [ "Replace 'foldl' with 'foldl''"
        , "Use 'foldr (flip . f)` instead of 'foldl f'"
        ]
    & severityL .~ Error
    & categoryL %~ (Category.spaceLeak `NE.cons`)

lenPatAst :: PatternAst
lenPatAst = range
    (PatternAstConstant 0)
    (app
        (PatternAstName (mkBaseFoldableMeta "length") (?))
        PatternAstAnything
    )
