module Test.Stan.Inspection
    ( inspectionsSpec
    ) where

import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, shouldBe)

import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..), InspectionsMap)
import Stan.Inspection.All (getInspectionById)
import Stan.Inspection.Infinite (infiniteInspectionsMap)
import Stan.Inspection.Partial (partialInspectionsMap)

import qualified Data.HashMap.Strict as HM


inspectionsSpec :: Spec
inspectionsSpec = describe "Inspections by ID" $ do
    describe "Partial" $
        check partialInspectionsMap
    describe "Infinite" $
        check infiniteInspectionsMap
  where
    check :: InspectionsMap -> SpecWith (Arg Expectation)
    check insMap = forM_ (HM.toList insMap) $ \(insId, ins) ->
        it (toString $ unId insId <> ": " <> inspectionName ins) $
            getInspectionById insId `shouldBe` ins
