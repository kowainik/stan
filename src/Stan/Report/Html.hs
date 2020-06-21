{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

HTML to be generated in the report.
-}

module Stan.Report.Html
    ( stanHtml
    ) where

import Prelude hiding (div, head)
import Relude.Extra.Enum (universe)

import Clay (compact, renderWith)
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (ins, map, summary)

import Stan.Analysis (Analysis (..))
import Stan.Analysis.Pretty (AnalysisNumbers (..), ProjectHealth (..), analysisToNumbers,
                             prettyHealth, toProjectHealth)
import Stan.Analysis.Summary (Summary (..), createSummary)
import Stan.Category (Category (..))
import Stan.Config (Config, ConfigP (..))
import Stan.Config.Pretty (ConfigAction, configActionClass, configToTriples, prettyConfigAction)
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
import qualified Data.Text as T
import qualified Text.Blaze.Html5.Attributes as A


stanHtml :: Analysis -> Config -> [Text] -> StanEnv -> ProjectInfo -> Html
stanHtml an config warnings env project =
    docTypeHtml (stanHead >> stanBody)
  where
    stanBody :: Html
    stanBody = body $ do
        stanHeader
        stanMain an config warnings env project
        stanFooter
        stanJs

stanHeader :: Html
stanHeader = header ! (A.class_ "centre") $ do
    divClass "row" (h1 "Stan Report")
    nav ! (A.class_ "row") $ do
      navItem "General Info"
      navItem "Observations"
      navItem "Configurations"
      navItem "Report Explained"
  where
    navItem :: Text -> Html
    navItem h = divClass "col-3 nav-item"
        (a ! (A.href $ fromText $ "#" <> hToId h) $ toHtml h)

stanMain :: Analysis -> Config -> [Text] -> StanEnv -> ProjectInfo -> Html
stanMain an config warnings env project = main  ! (A.class_ "container") $ do
    divClass "row" (p "This is Haskell Static Analysis report by Stan.")
    divIdClassH "Stan Info" "row" (stanInfo env)
    divIdClass "general-info" "row" $ do
        divIdClassH "Project Info" "col-6" (stanProject project)
        divIdClassH "Analysis Info" "col-6" (stanAnalysis analysisNumbers)
    divIdClassH "Static Analysis Summary" "row" (stanSummary an analysisNumbers)
    -- divIdClassH "Graphs" "row" (p_ "Maybe later")
    divIdClassH "Observations" "row" (stanObservations an)
    divIdClassH "Configurations" "row" (stanConfig an config warnings)
    -- divIdClassH "Summary" "row" (p_ "Later")
    divIdClassH "Report Explained" "" $ do
        divIdClassH "Inspections" "row" $ stanInspections (analysisInspections an)
        divIdClassH "Severity" "row" stanSeverityExplained
  where
    analysisNumbers :: AnalysisNumbers
    analysisNumbers = analysisToNumbers an

stanInfo :: StanEnv -> Html
stanInfo StanEnv{..} = do
    let StanVersion{..} = stanVersion
    let StanSystem{..} = stanSystem
    divClass "row" (blockP "General information about Stan and its compile time and runtime environments: how and where it was built and executed")
    divClass "col-10" $
        table ! (A.class_ "border-shadow" <> A.style "table-layout:fixed") $ do
            colgroup (col ! (A.style "width:25%") >> col)
            tr2 "Stan Version"
            tableRow "Version"       svVersion
            tableRow "Git Revision"  svGitRevision
            tableRow "Release Date"  svCommitDate
            tr2 "System Info"
            tableRow "Operating System" ssOs
            tableRow "Architecture"     ssArch
            tableRow "Compiler"         ssCompiler
            tableRow "Compiler Version" ssCompilerVersion
            tr2 "Environment"
            tableRow "Environment Variables"    seEnvVars
            tableRow "TOML configuration files" (traverse_ toHtml seTomlFiles)
            tableRow "CLI arguments"            (List.unwords seCliArgs)
  where
    tr2 x = tr $ td ! (A.colspan "2" <> A.class_ "centre grey-bg") $ strong x

stanProject :: ProjectInfo -> Html
stanProject ProjectInfo{..} = do
    divClass "row" (blockP "Information about the analysed project")
    tableWithShadow "" $ do
        colgroup (col ! (A.class_ "info-name") >> col ! (A.class_ "info-data"))
        tableRow "Project name"  piName
        tableRow "Cabal Files"   (List.unwords piCabalFiles)
        tableRow "HIE Files Directory" piHieDir
        tableRow "Files Number" piFileNumber

stanAnalysis :: AnalysisNumbers -> Html
stanAnalysis AnalysisNumbers{..} = do
    divClass "row" (blockP "Short stats from the static analysis")
    tableWithShadow "" $ do
        tableRow "Modules"               anModules
        tableRow "LoC"                   anLoc
        tableRow "Extensions"            anExts
        tableRow "SafeHaskel Extensions" anSafeExts
        tableRow "Available inspections" (HM.size inspectionsMap)
        tableRow "Checked inspections"   anIns
        tableRow "Found Observations"    anFoundObs
        tableRow "Ignored Observations"  anIgnoredObs

stanSummary :: Analysis -> AnalysisNumbers -> Html
stanSummary analysis AnalysisNumbers{..} = do
    divClass "row" (blockP "Summary of the static analysis report")
    ul ! (A.class_ "col-10") $ do
        liSum $ do
            h4 (toHtml $ "Project health: " <> prettyHealth anHealth)
            span $ toHtml @Text $ fold
                [ "This number was calculated based on the total number of used inspections "
                , "and the number of triggered inspections in the project. The calculated number "
                , "also defines the overall project health status."
                ]
        liSum $ do
            h4 (toHtml $ "The project " <> showProjectHealth projectHealth)
            span $ toHtml $ showHealthConclusions projectHealth
        summary
  where
    projectHealth :: ProjectHealth
    projectHealth = toProjectHealth anHealth

    showProjectHealth :: ProjectHealth -> Text
    showProjectHealth = \case
        Unhealthy    -> "is unhealthy"
        LowHealth    -> "has low health"
        MediumHealth -> "has medium health"
        Healthy      -> "is healthy"

    showHealthConclusions :: ProjectHealth -> Text
    showHealthConclusions = fold . \case
        Unhealthy ->
            [ "According to the Stan analysis, the project has a lot of vulnerabilities. "
            , "But this also means that there is a room for improving code quality! "
            , "Don't give up and continue doing great work!"
            ]
        LowHealth ->
            [ "According to the Stan analysis, the project has issues of a different variety. But you can improve that! "
            , "Stan provides solutions to the observed problems to help you improve the code quality."
            ]
        MediumHealth ->
            [ "Stan discovered several potential issues in the project. "
            , "Nice job, the overall project quality is good. And you can easily make it even better!"
            ]
        Healthy ->
            [ "Excellent work! Stan haven't found any vulnerabilities in the code."
            ]

    summary :: Html
    summary = case createSummary analysis of
        Nothing -> liSum $ do
            h4 "Congratulations! Your project has zero vulnerabilities!"
            span "Stan carefully run all configured inspection and found 0 observations and vulnerabilities to the project."
        Just Summary{..} -> do
            liSum $ do
                h4 $ toHtml ("Watch out for " <> unId summaryInspectionId)
                span $ do
                    toHtml @Text "By the result of Stan analysis, the most common inspection for this project is "
                    inspectionLink summaryInspectionId
            liSum $ do
                h4 $ toHtml ("Vulnerable module: " <> unModuleName summaryModule)
                span $ do
                    toHtml @Text "The "
                    code (toHtml $ unModuleName summaryModule)
                    toHtml @Text " module is the most vulnerable one in the project, as it got the most number of observations"
            liSum $ do
                h4 (toHtml $ "Popular category: " <> unCategory summaryCategory)
                categories "inline" $ one summaryCategory
                toHtml @Text "The project has the most problems with inspections from this category"
            liSum $ do
                h4 $ toHtml ("Severity: " <> show @Text summarySeverity)
                toHtml @Text "The highest severity of found vulnerabilities is "
                severity (show @Text summarySeverity)

    liSum :: Html -> Html
    liSum = li ! (A.class_ "sum")

stanObservations :: Analysis -> Html
stanObservations Analysis{..} = do
    divClass "row" (blockP "List of found vulnerabilities per file")
    sequence_ $ map stanPerFile $
        filter (not . null . fileInfoObservations) $ Map.elems analysisFileMap

stanPerFile :: FileInfo -> Html
stanPerFile FileInfo{..} = divIdClass "file" "row" $ do
    h3 ! (A.class_ "grey-bg") $ toHtml $ "📄 " <> fileInfoPath
    ul $ do
        li $ tableWithShadow "col-6" $ do
            tableRow "Module" $ code $ toHtml $ unModuleName fileInfoModuleName
            tableRow "Lines of Code" fileInfoLoc
        li $ divClass "extensions" $ do
            stanExtensions ".cabal" (extensionsToText fileInfoCabalExtensions)
            stanExtensions "module" (extensionsToText fileInfoExtensions)
        li ! (A.class_ "col-12 obs-li") $ divClass "observations col-12" $ do
            h4 "Observations"
            traverse_ stanObservation fileInfoObservations

stanExtensions :: Text -> [Text] -> Html
stanExtensions from exts = divClass "col-6" $ do
    button ! (A.class_ "collapsible") $ toHtml $ "Extensions from " <> from
    ol ! (A.class_ "content") $ sequence_ $ map (li . toHtml) exts

inspectionLink :: Id Inspection -> Html
inspectionLink ins = a ! A.class_ "ins-link" ! A.href (fromText $ "#" <> insId) $ toHtml insId
  where
    insId :: Text
    insId = unId ins

stanObservation :: Observation -> Html
stanObservation o@Observation{..} = divIdClass (unId observationId) "observation row" $ do
    general
    pre $ toHtml (unlines $ prettyObservationSource False o)
    solutionsDiv inspection
  where
    general = divClass "observation-general" $ tableWithShadow "" $ do
        tableR "ID"            (unId observationId)
        tableR "Severity"      (severityFromIns inspection)
        tableR "Description"   (inspectionDescription inspection)
        tableR "Inspection ID" (inspectionLink observationInspectionId)
        tableR "Category"      (categories "inline" $ inspectionCategory inspection)
        tableR "File"          observationFile

    tableR :: ToMarkup a => Text -> a -> Html
    tableR name val = tr $ do
        td ! (A.class_ "info-name very-light-bg") $ toHtml name
        td ! (A.class_ "info-data") $ toHtml val

    inspection :: Inspection
    inspection = getInspectionById observationInspectionId

severityFromIns :: Inspection -> Html
severityFromIns ins = severity $ show @Text $ inspectionSeverity ins

severity :: Text -> Html
severity severityTxt = span ! (A.class_ "severity") $ do
    span ! (A.class_ $ fromText $ "severity" <> severityTxt) $ toHtml @Text ""
    span ! (A.class_ "severityText") $ toHtml severityTxt

categories :: Text -> NonEmpty Category -> Html
categories cl cats = ul ! (A.class_ $ fromText $ "cats " <> cl) $ sequence_ $
    map ((li ! (A.class_ "cat")) . toHtml . unCategory) $ toList cats

solutionsDiv :: Inspection -> Html
solutionsDiv ins = memptyIfTrue (null solutions) $ divClass ("solutions border-shadow") $ do
    h4 "Possible solutions"
    uList solutions
  where
    solutions :: [Text]
    solutions = inspectionSolution ins

stanInspections :: HashSet (Id Inspection) -> Html
stanInspections ins = do
    divClass "row" (blockP "List of Inspections used for analysing the project")
    div $ sequence_ $ map stanInspection $ sortWith unId $ toList ins

stanInspection :: Id Inspection -> Html
stanInspection (getInspectionById -> ins@Inspection{..}) = do
    button ! A.class_ "collapsible" ! A.id (fromText insId) $
      toHtml ("Explore Inspection " <> insId)
    divClass "content row" $ divIdClass (insId <> "-content") "inspection col-12" $ do
        h3 $ toHtml ("Inspection " <> insId)
        p $ strong $ toHtml inspectionName
        p $ em $ toHtml inspectionDescription
        div (severityFromIns ins)
        div (categories "" inspectionCategory)
        solutionsDiv ins
  where
    insId :: Text
    insId = unId inspectionId

stanConfig :: Analysis -> Config -> [Text] -> Html
stanConfig Analysis{..} config warnings = divClass "col-12" $ do
    divClass "row" (blockP "Description of the custom Stan configuration and explanation of how it was assembled")
    divClass "row" $ table $ do
        tr (th "Action" >> th "Filter" >> th "Scope")
        sequence_ $ map toRows (configToTriples config)
    divClass "ignored-observations row" $ do
        toUl ignoredIds "Ignored Observations"
            "These observations are flagged as ignored through the configurations and are not considered in the final report"
        toUl unknownIds "Unrecognised Observations"
            "Some observation IDs specified in the configurations are not found"
    divClass "config-warnings row" $ do
        h4 "Configuration Process Information"
        p $
            "Information and warnings that were gathered during the configuration assemble process. "
          <> "This helps to understand how different parts of the configurations were retrieved."
        uList warnings
  where
    toRows :: (ConfigAction, Text, Text) -> Html
    toRows (act, fil, sc) = tr !
      (A.class_ $ fromText $ configActionClass act) $ do
        td ! (A.class_ "centre") $ span $ strong $ toHtml $ prettyConfigAction act
        td $ toHtml fil
        td $ toHtml sc

    toUl :: [Id a] -> Text -> Text -> Html
    toUl ids headerTxt desc = memptyIfTrue (null ids) $ divClass "ignored-obs" $ do
        h4 $ toHtml headerTxt
        p $ toHtml desc
        uList $ map unId ids

    ignoredIds, unknownIds :: [Id Observation]
    (ignoredIds, unknownIds) = ignoredObservations
        (configIgnored config)
        analysisIgnoredObservations

stanSeverityExplained :: Html
stanSeverityExplained = do
    divClass "col-5" $
        blockP "We are using the following severity system to indicate the observation level"

    tableWithShadow "col-7" $ do
        tr ! greyBg $ (th "Severity" >> th "Description")
        sequence_ $ map toSeverityRow (universe @Severity)
  where
    toSeverityRow :: Severity -> Html
    toSeverityRow s = tr $ do
        td (severity $ show s)
        td (toHtml $ severityDescription s)

stanFooter :: Html
stanFooter = footer $ do
    divClass "container" $ do
        divClass "row footer-link" $ do
            span "This report was generated by "
            a ! (A.href "https://github.com/kowainik/stan") $
                toHtml @Text "Stan — Haskell Static Analysis Tool."
        divClass "row footer-link" $ do
            span "Stan is created and maintained by "
            a ! (A.href "https://kowainik.github.io") $ toHtml @Text "Kowainik"
    nav ! (A.class_ "row centre") $ h3 $ strong "© Kowainik 2020"

stanHead :: Html
stanHead = head $ do
    meta ! (A.httpEquiv "Content-Type" <> A.content "text/html; charset=UTF-8")
    meta ! (A.httpEquiv "X-UA-Compatible" <> A.content "IE=Edge")
    nameContent "viewport" "width=device-width, initial-scale=1.0"
    nameContent "description" "Stan Report"
    nameContent "keywords" "Haskell, Static Analysis"
    nameContent "author" "Kowainik"
    title "Stan Report"

    style (toHtml $ renderWith compact [] stanCss)
  where
    nameContent x y = meta ! (A.name x <> A.content y)

stanJs :: Html
stanJs = script $ toHtml $ List.unlines
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

divClass :: Text -> Html -> Html
divClass c = div ! (A.class_ (fromText c))

divIdClass :: Text -> Text -> Html -> Html
divIdClass aId c = div ! (A.id (fromText aId) <> A.class_ (fromText c))

divIdClassH :: Text -> Text -> Html -> Html
divIdClassH h c rest = divIdClass (hToId h) c (h2 (toHtml h) >> rest)

blockP :: Text -> Html
blockP = blockquote . p . toHtml

tableRow :: ToMarkup a => Text -> a -> Html
tableRow name val = tr $ do
    td ! (A.class_ "info-name") $ toHtml name
    td ! (A.class_ "info-data very-light-bg") $ toHtml val

tableWithShadow :: Text -> Html -> Html
tableWithShadow cl = table ! (A.class_ $ fromText $ "border-shadow " <> cl)

uList :: ToMarkup a => [a] -> Html
uList = ul . traverse_ (li . toHtml)

greyBg :: Attribute
greyBg = A.class_ "grey-bg"

hToId :: Text -> Text
hToId = T.intercalate "-" . map T.toLower . words

fromText :: IsString s => Text -> s
fromText = fromString . toString
