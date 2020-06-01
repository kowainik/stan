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

      -- * All inspections
    , antiPatternInspectionsMap
    ) where

import Relude.Extra.Lens ((.~))
import Relude.Extra.Tuple (mapToFst)

import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..), InspectionAnalysis (..), InspectionsMap, descriptionL,
                        solutionL)
import Stan.NameMeta (mkBaseFoldableMeta)
import Stan.Pattern.Ast (PatternAst (..), app, range)
import Stan.Pattern.Edsl (PatternBool (..))
import Stan.Severity (Severity (PotentialBug))

import qualified Stan.Category as Category


-- | All anti-pattern 'Inspection's map from 'Id's.
antiPatternInspectionsMap :: InspectionsMap
antiPatternInspectionsMap = fromList $ map (mapToFst inspectionId)
    [ stan0201
    ]

-- | Smart constructor to create anti-pattern 'Inspection'.
mkAntiPatternInspection :: Id Inspection -> Text -> PatternAst -> Inspection
mkAntiPatternInspection insId name pat = Inspection
    { inspectionId = insId
    , inspectionName = "Anti-pattern: " <> name
    , inspectionDescription = ""
    , inspectionSolution = []
    , inspectionCategory = Category.antiPattern :| []
    , inspectionSeverity = PotentialBug
    , inspectionAnalysis = FindAst pat
    }

-- | 'Inspection' — @[0 .. length xs]@ @STAN-0201@.
stan0201 :: Inspection
stan0201 = mkAntiPatternInspection (Id "STAN-0201") "[0 .. length xs]" lenPatAst
    & descriptionL .~ "Creating a list with wrong number of indices"
    & solutionL .~
        [ "Replace '[0 .. length xs]' with '[0 .. length xs - 1]'"
        , "Use 'zip [0 ..] xs` to work with list of pairs: index and element"
        ]

lenPatAst :: PatternAst
lenPatAst = range
    (PatternAstConstant 0)
    (app
        (PatternAstName (mkBaseFoldableMeta "length") (?))
        PatternAstAnything
    )
