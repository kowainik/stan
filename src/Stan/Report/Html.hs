{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE NoOverloadedStrings #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

HTML to be generated in the report.
-}

module Stan.Report.Html
    ( stanHtml
    ) where

import Relude.Extra.Enum (universe)

import Clay (compact, renderWith)
import Data.Char (toLower)
import Html hiding (Summary)

import Stan.Analysis (Analysis (..))
import Stan.Analysis.Pretty (AnalysisNumbers (..), ProjectHealth (..), analysisToNumbers,
                             prettyHealth, toProjectHealth)
import Stan.Analysis.Summary (Summary (..), createSummary)
import Stan.Category (Category (..))
import Stan.Config (Config, ConfigP (..))
import Stan.Config.Pretty (configActionClass, configToTriples, prettyConfigAction)
import Stan.Core.Id (Id (..))
import Stan.Core.ModuleName (ModuleName (..))
import Stan.FileInfo (FileInfo (..), extensionsToText)
import Stan.Info (ProjectInfo (..), StanEnv (..), StanSystem (..), StanVersion (..), stanSystem,
                  stanVersion)
import Stan.Inspection (Inspection (..))
import Stan.Inspection.All (getInspectionById, inspectionsMap)
import Stan.Observation (Observation (..), ignoredObservations, prettyObservationSource)
import Stan.Report.Css (stanCss)
import Stan.Severity (Severity (..), severityDescription)

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Html.Attribute as A


stanHtml an (config :: Config) warnings env project =
    doctype_ # html_ (stanHead # stanBody)
  where
    stanBody = body_
        ( stanHeader
        # stanMain an config warnings env project
        # stanFooter
        # stanJs
        )

stanHeader = header_A (A.class_ "centre")
    ( divClass "row" (h1_ "Stan Report")
    # nav_A (A.class_ "row")
        ( navItem "General Info"
        # navItem "Observations"
        # navItem "Configurations"
        # navItem "Report Explained"
        )
    )
  where
    navItem h = divClass "col-3 nav-item" (a_A (A.href_ $ "#" <> hToId h) h)

stanMain an config warnings env project = main_A (A.class_ "container")
    ( divClass "row" (p_ "This is Haskell Static Analysis report by Stan.")
    # divIdClassH "Stan Info" "row" (stanInfo env)
    # divIdClass "general-info" "row"
        ( divIdClassH "Project Info" "col-6" (stanProject project)
        # divIdClassH "Analysis Info" "col-6" (stanAnalysis analysisNumbers)
        )
    # divIdClassH "Static Analysis Summary" "row" (stanSummary an analysisNumbers)
    -- # divIdClassH "Graphs" "row" (p_ "Maybe later")
    # divIdClassH "Observations" "row" (stanObservations an)
    # divIdClassH "Configurations" "row" (stanConfig an config warnings)
    -- # divIdClassH "Summary" "row" (p_ "Later")
    # divIdClassH "Report Explained" ""
        ( divIdClassH "Inspections" "row"
            ( divClass "row" (blockP "List of Inspections used for analysing the project")
            # stanInspections (analysisInspections an)
            )
        # divIdClassH "Severity" "row" stanSeverityExplained
        )
    )
  where
    analysisNumbers :: AnalysisNumbers
    analysisNumbers = analysisToNumbers an

stanInfo StanEnv{..} =
    let StanVersion{..} = stanVersion in
    let StanSystem{..} = stanSystem in
    ( divClass "row" (blockP "General information about Stan and its compile time and runtime environments: how and where it was built and executed")
    # divClass "col-10"
        ( table_A (A.class_ "border-shadow" # A.style_ "table-layout:fixed")
            ( colgroup_ (col_A (A.style_ "width:25%") # col_)
            # tr2 "Stan Version"
            # tableRow "Version"       svVersion
            # tableRow "Git Revision"  svGitRevision
            # tableRow "Release Date"  svCommitDate
            # tr2 "System Info"
            # tableRow "Operating System" ssOs
            # tableRow "Architecture"     ssArch
            # tableRow "Compiler"         ssCompiler
            # tableRow "Compiler Version" ssCompilerVersion
            # tr2 "Environment"
            # tableRow "Environment Variables"    seEnvVars
            # tableRow "TOML configuration files" seTomlFiles
            # tableRow "CLI arguments"            (List.unwords seCliArgs)
            )
        )
    )
  where
    tr2 x = tr_ $ td_A (A.colspan_ (2 :: Int) # A.class_ "centre grey-bg") $ strong_ x

stanProject ProjectInfo{..} =
    ( divClass "row" (blockP "Information about the analysed project")
    # tableWithShadow ""
        ( colgroup_ (col_A (A.class_ "info-name") # col_A (A.class_ "info-data"))
        # tableRow "Project name"  piName
        # tableRow "Cabal Files"   (List.unwords piCabalFiles)
        # tableRow "HIE Files Directory" piHieDir
        # tableRow "Files Number" piFileNumber
        )
    )

stanAnalysis AnalysisNumbers{..} =
    ( divClass "row" (blockP "Short stats from the static analysis")
    # tableWithShadow ""
        ( tableRow "Modules"               anModules
        # tableRow "LoC"                   anLoc
        # tableRow "Extensions"            anExts
        # tableRow "SafeHaskel Extensions" anSafeExts
        # tableRow "Available inspections" (HM.size inspectionsMap)
        # tableRow "Checked inspections"   anIns
        # tableRow "Found Observations"    anFoundObs
        # tableRow "Ignored Observations"  anIgnoredObs
        )
    )

stanSummary analysis AnalysisNumbers{..} =
    ( divClass "row" (blockP "Summary of the static analysis report")
    # ul_A (A.class_ "col-10")
        ( li_A (A.class_ "sum")
            ( h4_ ("Project health: " # prettyHealth anHealth)
            # span_ "This was calculated TODO"
            )
        # li_A (A.class_ "sum")
            ( h4_ ("The project " # showProjectHealth (toProjectHealth anHealth))
            # span_ "Conclusions TODO"
            )
        # summary
        )
    )
  where
    showProjectHealth :: ProjectHealth -> Text
    showProjectHealth = toText . \case
        Unhealthy    -> "is unhealthy"
        LowHealth    -> "has low health"
        MediumHealth -> "has medium health"
        Healthy      -> "is healthy"

    summary = case createSummary analysis of
        Nothing -> Left $ li_A (A.class_ "sum")
            ( h4_ "Congratulations! Your project has zero vulnerabilities!"
            # span_ "Stan carefully run all configured inspection and found 0 observations and vulnerabilities to the project"
            )
        Just Summary{..} ->
            Right
            $ li_A (A.class_ "sum")
                ( h4_ ("Watch out for " # unId summaryInspectionId)
                # span_
                    ("By the result of Stan analysis, the most common inspection for this project is "
                    # inspectionLink summaryInspectionId)
                )
            # li_A (A.class_ "sum")
                ( h4_ ("Vulnerable module: " # unModuleName summaryModule)
                # span_ ("The " # code_ (unModuleName summaryModule) # " module is the most vulnerable one in the project, as it got the most number of observations")
                )
            # li_A (A.class_ "sum")
                ( h4_ ("Popular category: " # unCategory summaryCategory)
                # ( categories "inline" [summaryCategory]
                  # "The project has the most problems with inspections from this category"
                  )
                )
            # li_A (A.class_ "sum")
                ( h4_ ("Severity: " # show @Text summarySeverity)
                # ( "The highest severity of found vulnerabilities is "
                  # severity (show @Text summarySeverity)
                  )
                )

stanObservations Analysis{..} =
    ( divClass "row" (blockP "List of found vulnerabilities per file")
    # map stanPerFile
      ( filter (not . null . fileInfoObservations)
      $ Map.elems analysisFileMap
      )
    )

stanPerFile FileInfo{..} = divIdClass "file" "row"
    ( h3_A (A.class_ "grey-bg") ("📄 " # fileInfoPath)
    # ul_
        ( li_ (tableWithShadow "col-6"
            ( tableRow "Module" (code_ $ unModuleName fileInfoModuleName)
            # tableRow "Lines of Code" (show @Text fileInfoLoc)
            ))
        # li_ (divClass "extensions"
              ( stanExtensions ".cabal" (extensionsToText fileInfoCabalExtensions)
              # stanExtensions "module" (extensionsToText fileInfoExtensions)
              )
              )
        # li_A (A.class_ "col-12 obs-li") (divClass "observations col-12"
            ( h4_ "Observations" # map stanObservation (toList fileInfoObservations)))
        )
    )

stanExtensions from exts = divClass "col-6"
    ( button_A (A.class_ "collapsible") ("Extensions from " # from)
    # ol_A (A.class_ "content") (map li_ exts)
    )

inspectionLink ins = a_A (A.class_ "ins-link" # A.href_ (toText "#" <> insId)) insId
  where
    insId :: Text
    insId = unId ins

stanObservation o@Observation{..} = divIdClass (unId observationId) "observation row"
    ( general
    # pre_ (unlines $ prettyObservationSource False o)
    # solutionsDiv inspection
    )
  where
    general = divClass "observation-general" $ tableWithShadow ""
        ( tableR "ID"            (unId observationId)
        # tableR "Severity"      (severityFromIns inspection)
        # tableR "Description"   (inspectionDescription inspection)
        # tableR "Inspection ID" (inspectionLink observationInspectionId)
        # tableR "Category"      (categories "inline" $ inspectionCategory inspection)
        # tableR "File"          observationFile
        )

    tableR name val = tr_
        ( td_A (A.class_ "info-name very-light-bg") name
        # td_A (A.class_ "info-data") val
        )

    inspection :: Inspection
    inspection = getInspectionById observationInspectionId

severityFromIns ins = severity $ show @Text $ inspectionSeverity ins

severity severityTxt = span_A (A.class_ "severity")
    ( span_A (A.class_ $ toText "severity" <> severityTxt) ""
    # span_A (A.class_ "severityText") severityTxt
    )

categories cl cats = ul_A (A.class_ $ "cats " <> cl) $
    map (li_A (A.class_ "cat") . unCategory) $ toList cats

solutionsDiv ins = nothingIfTrue (null solutions)
    ( divClass ("solutions border-shadow")
        ( h4_ "Possible solutions"
        # ul_ (map li_ solutions)
        )
    )
  where
    solutions :: [Text]
    solutions = inspectionSolution ins

stanInspections = div_ . map stanInspection . sortWith unId . toList
stanInspection (getInspectionById -> ins@Inspection{..}) =
    button_A (A.class_ "collapsible" # A.id_ insId) ("Explore Inspection " # insId)
    # divClass "content row" ( divIdClass (insId <> toText "-content") "inspection col-12"
        ( h3_ ("Inspection " # unId inspectionId)
        # p_ (strong_ inspectionName)
        # p_ (em_ inspectionDescription)
        # div_ (severityFromIns ins)
        # div_ (categories "" inspectionCategory)
        # solutionsDiv ins
        )
    )
  where
    insId :: Text
    insId = unId inspectionId

stanConfig Analysis{..} (config :: Config) (warnings :: [Text]) = divClass "col-12 "
    ( divClass "row" (blockP "Description of the custom Stan configuration and explanation of how it was assembled")
    # divClass "row" (table_
        ( tr_ (th_ "Action" # th_ "Filter" # th_ "Scope")
        # map toRows (configToTriples config)
        ))
    # divClass "ignored-observations row"
        ( toUl ignoredIds "Ignored Observations"
            "These observations are flagged as ignored through the configurations and are not considered in the final report"
        # toUl unknownIds "Unrecognised Observations"
            "Some observation IDs specified in the configurations are not found"
        )
    # divClass "config-warnings row"
        ( h4_ "Configuration Process Information"
        # p_
            ( "Information and warnings that were gathered during the configuration assemble process. "
            # "This helps to understand how different parts of the configurations were retrieved."
            )
        # ul_ (map li_ warnings)
        )
    )
  where
    toRows (act, fil, sc) = tr_A (A.class_ $ configActionClass act)
      ( td_A (A.class_ "centre") (span_ $ strong_ $ prettyConfigAction act)
      # td_ fil
      # td_ sc
      )

    toUl ids header desc = nothingIfTrue (null ids) $ divClass "ignored-obs"
        ( h4_ header
        # p_ desc
        # ul_ (map (li_ . unId) ids)
        )

    ignoredIds, unknownIds :: [Id Observation]
    (ignoredIds, unknownIds) = ignoredObservations
        (configIgnored config)
        analysisIgnoredObservations

stanSeverityExplained =
      divClass "col-5"
        (blockP "We are using the following severity system to indicate the observation level")

    # tableWithShadow "col-7"
        ( tr_A greyBg (th_ "Severity" # th_ "Description")
        # map toSeverityRow (universe @Severity)
        )
  where
    toSeverityRow s = tr_
        ( td_ (severity $ show @Text s)
        # td_ (severityDescription s)
        )

stanFooter = footer_
    ( divClass "container"
        ( divClass "row footer-link"
            ( span_ "This report was generated by "
            # a_A (A.href_ "https://github.com/kowainik/stan") "Stan — Haskell Static Analysis Tool."
            )
        # divClass "row footer-link"
            ( span_ "Stan is created and maintained by "
            # a_A (A.href_ "https://kowainik.github.io") "Kowainik"
            )
        )
    # nav_A (A.class_ "row centre") (h3_ $ strong_ "© Kowainik 2020")
    )

stanHead = head_
    ( meta_A (A.httpEquiv_ "Content-Type" # A.content_ "text/html; charset=UTF-8")
    # meta_A (A.httpEquiv_ "X-UA-Compatible" # A.content_ "IE=Edge")
    # nameContent "viewport" "width=device-width, initial-scale=1.0"
    # nameContent "description" "Stan Report"
    # nameContent "keywords" "Haskell, Static Analysis"
    # nameContent "author" "Kowainik"
    # title_ "Stan Report"

    # style_ (Raw $ renderWith compact [] stanCss)
    )
  where
    nameContent x y = meta_A (A.name_ x # A.content_ y)

stanJs = script_ $ Raw $ List.unlines
    [ "var coll = document.getElementsByClassName(\"collapsible\");"
    , "var i;"
    , ""
    , "for (i = 0; i < coll.length; i++) {"
    , "  coll[i].addEventListener(\"click\", function() {"
    , "    this.classList.toggle(\"active\");"
    , "    var content = this.nextElementSibling;"
    , "    if (content.style.maxHeight){"
    , "      content.style.maxHeight = null;"
    , "    } else {"
    , "      content.style.maxHeight = content.scrollHeight + \"px\";"
    , "    }"
    , "  });"
    , "}"
    ]

divClass c = div_A (A.class_ c)
divIdClass i c = div_A (A.id_ i # A.class_ c)

divIdClassH h c rest = divIdClass (hToId h) c (h2_ h # rest)

blockP t = blockquote_ (p_ t)

tableRow name val = tr_
    ( td_A (A.class_ "info-name") name
    # td_A (A.class_ "info-data very-light-bg") val
    )

tableWithShadow cl = table_A (A.class_ $ "border-shadow " <> cl)

greyBg = A.class_ "grey-bg"

hToId :: String -> String
hToId = intercalate "-" . map (map toLower) . List.words

nothingIfTrue :: Bool -> a -> Maybe a
nothingIfTrue p a = if p then Nothing else Just a
