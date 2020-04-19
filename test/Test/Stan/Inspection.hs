module Test.Stan.Inspection
    ( inspectionsSpec
    ) where

import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, shouldBe)

import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..))
import Stan.Inspection.All (getInspectionById)
import Stan.Inspection.Infinite (infiniteInspections, infiniteInspectionsIds)
import Stan.Inspection.Partial (partialInspections, partialInspectionsIds)


inspectionsSpec :: Spec
inspectionsSpec = describe "Inspections by ID" $ do
    describe "Partial" $
        check partialInspections partialInspectionsIds
    describe "Infinite" $
        check infiniteInspections infiniteInspectionsIds
  where
    check :: [Inspection] -> [Id Inspection] -> SpecWith (Arg Expectation)
    check inss insIds = forM_ (zip inss insIds) $ \(ins, insId) ->
        it (toString $ unId insId <> ": " <> inspectionName ins) $
            getInspectionById insId `shouldBe` ins
