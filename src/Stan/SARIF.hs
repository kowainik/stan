{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Provides functions to convert @Stan@'s data types to equivalent SARIF ones.
-}

module Stan.SARIF
    ( toSARIF
    ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Lazy as Map
import Data.SARIF as SARIF
import qualified Data.Text as T
import Data.Version (showVersion)

import Paths_stan (version)
import Stan.Analysis (Analysis (..))
import Stan.Core.Id
import Stan.FileInfo
import Stan.Ghc.Compat
import Stan.Inspection
import Stan.Inspection.All (inspections)
import Stan.Observation
import Stan.Severity as Stan

-- | Represents @Stan@ as a `SARIF.Tool` value.
stanTool :: SARIF.Tool
stanTool = MkTool{
    toolDriver = defaultToolComponent{
        toolComponentName = Just "Stan",
        -- Haskell versions aren't valid semver versions
        toolComponentVersion = Just $ T.pack $ showVersion version,
        toolComponentInformationUri = Just "https://github.com/kowainik/stan/",
        toolComponentRules = Map.elems reportingDescriptors
    },
    toolExtensions = []
}

-- | `fileMapToArtifacts` @fileMap@ converts @fileMap@ to a list of
-- `SARIF.Artifact` values.
fileMapToArtifacts :: FileMap -> [SARIF.Artifact]
fileMapToArtifacts fm = map toArtifact $ Map.keys fm
    where toArtifact fp = MkArtifact{
                artifactLocation = MkArtifactLocation{
                    artifactLocationUri = T.pack fp
                },
                artifactMimeType = Nothing
            }

-- | `toLevel` @severity@ converts a @Stan@ `Severity` to a SARIF `Level`.
toLevel :: Severity -> Level
toLevel Style        = Note
toLevel Performance  = SARIF.Warning
toLevel PotentialBug = SARIF.Warning
toLevel Stan.Warning = SARIF.Warning
toLevel Stan.Error   = SARIF.Error

-- | `toReportingDescriptor` @inspection@ converts @inspection@ to
-- a `SARIF.ReportingDescriptor`.
toReportingDescriptor :: Inspection -> SARIF.ReportingDescriptor
toReportingDescriptor Inspection{..} =
    (defaultReportingDescriptor $ unId inspectionId){
        rdName = Nothing,
        rdShortDescription = Just $
            defaultMultiformatMessageString inspectionName,
        rdFullDescription = Just $
            defaultMultiformatMessageString inspectionDescription,
        -- TODO: make this useful
        rdHelpUri = Just "https://github.com/kowainik/stan/",
        rdHelp = Just $
            defaultMultiformatMessageString (T.unlines inspectionSolution),
        rdDefaultConfiguration = Just $ defaultReportingConfiguration{
            rcLevel = Just $ toLevel inspectionSeverity
        } -- ,
        -- tricky, because Stan isn't currently using aeson
        -- rdProperties = Map.singleton "tags" $ _ $ map (toJSON . _) inspectionCategory
    }

-- | `reportingDescriptors` is a `Map.Map` of `SARIF.ReportingDescriptor` which
-- correspond to @Stan@ `Inspection`s, indexed by their Id.
reportingDescriptors :: Map.Map Text SARIF.ReportingDescriptor
reportingDescriptors = Map.fromList
    [ (rdId rd, rd) | rd <- map toReportingDescriptor inspections ]

-- | `observationToResult` @observation@ converts an @observation@ to
-- a `SARIF.Result`.
observationToResult :: Observation -> SARIF.Result
observationToResult Observation{..} =
    let mrd = Map.lookup (unId observationInspectionId) reportingDescriptors
    in MkResult{
        resultRuleId = unId observationInspectionId,
        resultLevel = Nothing,
        resultMessage = defaultMultiformatMessageString $
            fromMaybe "A problem was detected here." $
                mrd >>= fmap mmsText . rdFullDescription,
        resultLocations = [
            MkLocation{
                locationPhysicalLocation = Just MkPhysicalLocation{
                    physicalLocationArtifactLocation = MkArtifactLocation{
                        artifactLocationUri = T.pack observationFile
                    },
                    physicalLocationRegion = MkRegion{
                        regionStartLine = srcSpanStartLine observationSrcSpan,
                        regionStartColumn = srcSpanStartCol observationSrcSpan,
                        regionEndLine = srcSpanEndLine observationSrcSpan,
                        regionEndColumn = srcSpanEndCol observationSrcSpan
                    }
                }
            }
        ]
    }

-- | `toSARIF` @analysis@ converts an @analysis@ to a SARIF log and encodes it
-- as JSON which is returned as a `LBS.ByteString`.
toSARIF :: Analysis -> LBS.ByteString
toSARIF Analysis{..} = encodeSarifAsLBS $ defaultLog{
    logRuns = [
        MkRun{
            runTool = stanTool,
            runArtifacts = fileMapToArtifacts analysisFileMap,
            runResults = map observationToResult $ toList analysisObservations
        }
    ]
}
