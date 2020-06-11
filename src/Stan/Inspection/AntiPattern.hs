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
      -- *** Anti-pattern 'Data.ByteString.Char8.pack'
    , stan0203
      -- *** Anti-pattern slow 'size' for 'HashMap'
    , stan0204
      -- *** Anti-pattern slow 'size' for 'HashSet'
    , stan0205
      -- *** Anti-pattern: Lazy fields
    , stan0206
      -- *** Anti-pattern: Foldable methods on tuples, 'Maybe', 'Either'
    , stan0207
      -- *** Anti-pattern: slow 'length' for 'Text'
    , stan0208
      -- *** Anti-pattern: Slow 'nub' for lists
    , stan0209
      -- *** Anti-pattern: Slow 'for_' on ranges
    , stan0210

      -- * All inspections
    , antiPatternInspectionsMap
    ) where

import Relude.Extra.Lens ((%~), (.~))
import Relude.Extra.Tuple (fmapToFst)

import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..), InspectionAnalysis (..), InspectionsMap, categoryL,
                        descriptionL, severityL, solutionL)
import Stan.NameMeta (NameMeta (..), mkBaseFoldableMeta, mkBaseOldListMeta, textNameFrom,
                      unorderedNameFrom)
import Stan.Pattern.Ast (PatternAst (..), app, namesToPatternAst, range)
import Stan.Pattern.Edsl (PatternBool (..))
import Stan.Pattern.Type (PatternType, foldableMethodsPatterns, foldableTypesPatterns, listPattern,
                          textPattern, (|->), (|::))
import Stan.Severity (Severity (..))

import qualified Data.List.NonEmpty as NE
import qualified Stan.Category as Category


-- | All anti-pattern 'Inspection's map from 'Id's.
antiPatternInspectionsMap :: InspectionsMap
antiPatternInspectionsMap = fromList $ fmapToFst inspectionId
    [ stan0201
    , stan0202
    , stan0203
    , stan0204
    , stan0205
    , stan0206
    , stan0207
    , stan0208
    , stan0209
    , stan0210
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
  where
    lenPatAst :: PatternAst
    lenPatAst = range
        (PatternAstConstant 0)
        (app
            (PatternAstName (mkBaseFoldableMeta "length") (?))
            (?)
        )

-- | 'Inspection' — 'foldl' @STAN-0202@.
stan0202 :: Inspection
stan0202 = mkAntiPatternInspection (Id "STAN-0202") "foldl"
    (FindAst $ PatternAstName (mkBaseFoldableMeta "foldl") (?))
    & descriptionL .~ "Usage of space-leaking function 'foldl'"
    & solutionL .~
        [ "Replace 'foldl' with 'foldl''"
        , "Use 'foldr (flip . f)` instead of 'foldl f'"
        ]
    & severityL .~ Error
    & categoryL %~ (Category.spaceLeak `NE.cons`)

-- | 'Inspection' — 'Data.ByteString.Char8.pack' @STAN-0203@.
stan0203 :: Inspection
stan0203 = mkAntiPatternInspection (Id "STAN-0203") "Data.ByteString.Char8.pack"
    (FindAst $ PatternAstName packNameMeta (?))
    & descriptionL .~ "Usage of 'pack' function that doesn't handle Unicode characters"
    & solutionL .~
        [ "Convert to 'Text' and use 'encodeUtf8' from 'Data.Text.Encoding'"
        , "Use packages that provide UTF-8 encoding functions: 'utf8-string', 'relude'"
        ]
    & severityL .~ Error
  where
    packNameMeta :: NameMeta
    packNameMeta = NameMeta
        { nameMetaPackage    = "bytestring"
        , nameMetaModuleName = "Data.ByteString.Char8"
        , nameMetaName       = "pack"
        }

-- | 'Inspection' — slow 'Data.HashMap.Strict.size' and 'length' @STAN-0204@.
stan0204 :: Inspection
stan0204 = mkAntiPatternInspection (Id "STAN-0204") "HashMap size"
    (FindAst $ namesToPatternAst pats)
    & descriptionL .~ "Usage of 'size' or 'length' for 'HashMap' that runs in linear time"
    & solutionL .~
        [ "Switch to 'Map' from 'containers' if this data type works for you"
        ]
    & severityL .~ Performance
  where
    pats :: NonEmpty (NameMeta, PatternType)
    pats = (sizeNameMeta, (?))
        :| [(mkBaseFoldableMeta "length", hmPat)]

    sizeNameMeta :: NameMeta
    sizeNameMeta = "size" `unorderedNameFrom` "Data.HashMap.Base"

    hm :: NameMeta
    hm = "HashMap" `unorderedNameFrom` "Data.HashMap.Base"

    hmPat :: PatternType
    hmPat = (hm |:: [(?), (?)]) |-> (?)

-- | 'Inspection' — slow 'Data.HashMap.Strict.size' @STAN-0205@.
stan0205 :: Inspection
stan0205 = mkAntiPatternInspection (Id "STAN-0205") "HashSet size"
           (FindAst $ namesToPatternAst pats)
    & descriptionL .~ "Usage of 'size' or 'length' for 'HashSet' that runs in linear time"
    & solutionL .~
        [ "Switch to 'Set' from 'containers' if this data type works for you"
        ]
    & severityL .~ Performance
  where
    pats :: NonEmpty (NameMeta, PatternType)
    pats = (sizeNameMeta, (?))
        :| [(mkBaseFoldableMeta "length", hsPat)]

    sizeNameMeta :: NameMeta
    sizeNameMeta = "size" `unorderedNameFrom` "Data.HashSet.Base"

    hs :: NameMeta
    hs = "HashSet" `unorderedNameFrom` "Data.HashSet.Base"

    hsPat :: PatternType
    hsPat = (hs |:: [(?)]) |-> (?)

-- | 'Inspection' — missing fixity declaration @STAN-0206@.
stan0206 :: Inspection
stan0206 = Inspection
    { inspectionId = Id "STAN-0206"
    , inspectionName = "Data types with non-strict fields"
    , inspectionDescription =
        "Defining lazy fields in data types can lead to unexpected space leaks"
    , inspectionSolution =
        [ "Add '!' before the type, e.g. !Int or !(Maybe Bool)"
        , "Enable the 'StrictData' extension: {-# LANGUAGE StrictData #-}"
        ]
    , inspectionCategory = Category.spaceLeak :| [Category.syntax]
    , inspectionSeverity = Performance
    , inspectionAnalysis = LazyField
    }

-- | 'Inspection' — 'Foldable' methods on possibly error-prone structures @STAN-0207@.
stan0207 :: Inspection
stan0207 = mkAntiPatternInspection
    (Id "STAN-0207")
    "Foldable methods on possibly error-prone structures"
    (FindAst $ namesToPatternAst allPatterns)
    & descriptionL .~ "Usage of Foldable methods on (,), Maybe, Either"
    & solutionL .~
        [ "Use more explicit functions with specific monomorphic types"
        ]
  where
    allPatterns :: NonEmpty (NameMeta, PatternType)
    allPatterns = do  -- Monad for NonEmpty
        t <- foldableTypesPatterns
        (method, mkType) <- foldableMethodsPatterns
        pure (method, mkType t)

-- | 'Inspection' — slow 'length' for 'Data.Text' @STAN-0208@.
stan0208 :: Inspection
stan0208 = mkAntiPatternInspection (Id "STAN-0208") "Slow 'length' for Text"
           (FindAst $ PatternAstName lenNameMeta (textPattern |-> (?)))
    & descriptionL .~ "Usage of 'length' for 'Text' that runs in linear time"
    & solutionL .~
        [ "Switch to 'ByteString' from 'bytesting' if this data type works for you"
        ]
    & severityL .~ Performance
  where
    lenNameMeta :: NameMeta
    lenNameMeta = "length" `textNameFrom` "Data.Text"

-- | 'Inspection' — slow 'nub' for lists @STAN-0209@.
stan0209 :: Inspection
stan0209 = mkAntiPatternInspection (Id "STAN-0209") "Slow 'nub' for lists"
           (FindAst $ PatternAstName (mkBaseOldListMeta "nub") $ listPattern |-> listPattern)
    & descriptionL .~ "Usage of 'nub' on lists that runs in quadratic time"
    & solutionL .~
        [ "Switch list to 'Set' from 'containers' if this data type works for you"
        ]
    & severityL .~ Performance

-- | 'Inspection' — slow 'for_' and 'forM_' for ranges @STAN-0210@.
stan0210 :: Inspection
stan0210 = mkAntiPatternInspection (Id "STAN-0210") "Slow 'for_' on ranges" (FindAst pat)
    & descriptionL .~ "Usage of 'for_' or 'forM_' on numerical ranges is slow"
    & solutionL .~
        [ "Use 'loop' library for fast monadic looping"
        ]
    & severityL .~ Performance
  where
    pat :: PatternAst
    pat = app (forPattern) (range (?) (?))

    forPattern :: PatternAst
    forPattern = PatternAstOr
        (PatternAstName (mkBaseFoldableMeta "for_") forType)
        (PatternAstName (mkBaseFoldableMeta "forM_") forType)

    forType :: PatternType
    forType = listPattern |-> ((?) |-> (?)) |-> (?)
