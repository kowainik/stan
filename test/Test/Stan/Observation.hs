module Test.Stan.Observation
    ( observationSpec
    ) where

import SrcLoc (mkRealSrcLoc, mkRealSrcSpan)
import Test.Hspec (Spec, describe, it, shouldBe)

import Stan.Core.Id (Id (..))
import Stan.Inspection (inspectionId)
import Stan.Inspection.Partial (stan0001)
import Stan.Observation (Observation, mkObservationId)


observationSpec :: Spec
observationSpec = describe "Observation" $
    it "calculates Observation Id properly" $
        testObservationId `shouldBe` Id "OBS-STAN-0001-bbjjJQ-10:42"
  where
    testObservationId :: Id Observation
    testObservationId = mkObservationId
        (inspectionId stan0001)
        "Test.Module.Name"
        $ mkRealSrcSpan
            (mkRealSrcLoc "src/Test/Module/Name.hs" 10 42)
            (mkRealSrcLoc "src/Test/Module/Name.hs" 10 42)
