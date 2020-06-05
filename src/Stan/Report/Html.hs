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

import Clay (compact, renderWith)
import Data.Char (toLower)
import Html (Raw (..), a_A, body_, div_, div_A, doctype_, em_, footer_, h1_, h2_, h3_, h4_, head_,
             header_A, html_, li_, main_A, meta_A, nav_A, p_, pre_, span_, span_A, strong_, style_,
             table_, td_, td_A, th_, title_, tr_, tr_A, ul_, ( # ))

import Stan.Analysis (Analysis (..))
import Stan.Analysis.Pretty (AnalysisNumbers (..), analysisToNumbers)
import Stan.Category (Category (..))
import Stan.Config (Config, ConfigP (..))
import Stan.Config.Pretty (configActionClass, configToTriples, prettyConfigAction)
import Stan.Core.Id (Id (..))
import Stan.Core.ModuleName (ModuleName (..))
import Stan.FileInfo (FileInfo (..))
import Stan.Info (ProjectInfo (..), StanEnv (..), StanSystem (..), StanVersion (..), stanSystem,
                  stanVersion)
import Stan.Inspection (Inspection (..))
import Stan.Inspection.All (getInspectionById, inspectionsMap)
import Stan.Observation (Observation (..), ignoredObservations, prettyObservationSource)
import Stan.Report.Css (stanCss)

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
        )

stanHeader = header_A (A.class_ "centre")
    ( divClass "row" (h1_ "Stan Report")
    # nav_A (A.class_ "row")
        ( navItem "General Info"
        # navItem "Observations"
        # navItem "Configurations"
        # navItem "Inspections"
        )
    )
  where
    navItem h = divClass "col-3 nav-item" (a_A (A.href_ $ "#" <> hToId h) h)

stanMain an config warnings env project = main_A (A.class_ "container")
    ( divClass "row" (p_ "This is Haskell Static Analysis report by Stan.")
    # divIdClassH "Stan Info" "row" (stanInfo env)
    # divIdClass "general-info" "row"
        ( divIdClassH "Project Info" "col-6" (stanProject project)
        # divIdClassH "Analysis Info" "col-6" (stanAnalysis an)
        )
    -- # divIdClassH "Graphs" "row" (p_ "Maybe later")
    # divIdClassH "Observations" "row" (stanObservations an)
    # divIdClassH "Configurations" "row" (stanConfig an config warnings)
    -- # divIdClassH "Summary" "row" (p_ "Later")
    # divIdClassH "Inspections" "row" (stanInspections $ analysisInspections an)
    )

stanInfo StanEnv{..} =
    let StanVersion{..} = stanVersion in
    let StanSystem{..} = stanSystem in divClass "col-9" $
    table_
        ( tr2 "Stan Version"
        # tr_ (td_ "v"            # td_ svVersion)
        # tr_ (td_ "Git Revision" # td_ svGitRevision)
        # tr_ (td_ "Release Date" # td_ svCommitDate)
        # tr2 "System Info"
        # tr_ (td_ "Operating System" # td_ ssOs)
        # tr_ (td_ "Architecture"     # td_ ssArch)
        # tr_ (td_ "Compiler"         # td_ ssCompiler)
        # tr_ (td_ "Compiler Version" # td_ ssCompilerVersion)
        # tr2 "Environment"
        # tr_ (td_ "Environment Variables"   # td_ seEnvVars)
        # tr_ (td_ "TOML configuration files" # td_ seTomlFiles)
        # tr_ (td_ "CLI arguments"           # td_ (List.unwords seCliArgs))
        )
  where
    tr2 x = tr_ $ td_A (A.colspan_ (2 :: Int) # A.class_ "centre") $ strong_ x

stanProject ProjectInfo{..} = table_
    ( tr_ (td_ "Project name" # td_ piName)
    # tr_ (td_ "Cabal Files"  # td_ (List.unwords piCabalFiles))
    # tr_ (td_ "HIE Files Directory" # td_ piHieDir)
    # tr_ (td_ "Files Number" # td_ piFileNumber)
    )

stanAnalysis (analysisToNumbers -> AnalysisNumbers{..}) = table_
    ( tr_ (td_ "Modules" # td_ anModules)
    # tr_ (td_ "LoC"     # td_ anLoc)
    # tr_ (td_ "Extensions" # td_ anExts)
    # tr_ (td_ "SafeHaskel Extensions" # td_ anSafeExts)
    # tr_ (td_ "Available inspections" # td_ (HM.size inspectionsMap))
    # tr_ (td_ "Checked inspections" # td_ anIns)
    # tr_ (td_ "Found Observations" # td_ anFoundObs)
    # tr_ (td_ "Ignored Observations" # td_ anIgnoredObs)
    )

stanObservations Analysis{..} =
    map stanPerFile
    $ filter (not . null . fileInfoObservations)
    $ Map.elems analysisFileMap

stanPerFile FileInfo{..} = divIdClass "" "row" ( h3_ fileInfoPath # ul_
    ( li_ ("Module: " # unModuleName fileInfoModuleName)
    # li_ ("Lines of Code:" <> show fileInfoLoc)
    # li_ (divClass "observations"
      ( strong_ "Observations" # map stanObservation (toList fileInfoObservations)))
    )
    )

stanObservation o@Observation{..} = divIdClass (unId observationId) "observation col-12"
    ( general
    # pre_ (unlines $ prettyObservationSource False o)
    # solutionsDiv inspection
    )
  where
    general = divClass "observation-general" $ table_
        ( tr_ (td_ "ID " # td_ (strong_ $ unId observationId))
        # tr_ (td_ "Severity" # td_ (severity inspection))
        # tr_ (td_ "Description" # td_ (inspectionDescription inspection))
        # tr_ (td_ "Inspection ID" # td_ (a_A (A.href_ $ toText "#" <> insId) insId))
        # tr_ (td_ "Category" # td_ (categories inspection))
        # tr_ (td_ "File" # td_ observationFile)
        )

    inspection :: Inspection
    inspection = getInspectionById observationInspectionId

    insId :: Text
    insId = unId observationInspectionId

severity ins = span_A (A.class_ $ toText "severity" <> severityTxt) severityTxt
  where
    severityTxt :: Text
    severityTxt = show $ inspectionSeverity ins

categories ins = map (span_A (A.class_ "cat") . unCategory) $
    toList $ inspectionCategory ins

solutionsDiv ins = nothingIfTrue (null solutions)
    ( divClass "solutions"
        ( p_ "Possible solution"
        # ul_ (map li_ solutions)
        )
    )
  where
    solutions :: [Text]
    solutions = inspectionSolution ins

stanInspections = div_ . map stanInspection . sortWith unId . toList
stanInspection (getInspectionById -> ins@Inspection{..}) = divIdClass (unId inspectionId) "inspection"
    ( h3_ ("Inspection " # unId inspectionId)
    # p_ (strong_ inspectionName)
    # p_ (em_ inspectionDescription)
    # p_ (severity ins)
    # p_ (categories ins)
    # solutionsDiv ins
    )

stanConfig Analysis{..} (config :: Config) (warnings :: [Text]) = divClass "col-12 "
    ( divClass "row" (table_
        ( tr_ (th_ "Action" # th_ "Filter" # th_ "Scope")
        # map toRows (configToTriples config)
        ))
    # divClass "ignored-observations row"
        (toUl ignoredIds "Ignored Observations"
        # toUl unknownIds "Unrecognised Observations"
        )
    # divClass "config-warnings row"
        ( h4_ "Warnings"
        # ul_ (map li_ warnings)
        )
    )
  where
    toRows (act, fil, sc) = tr_A (A.class_ $ configActionClass act)
      ( td_A (A.class_ "centre") (span_ $ strong_ $ prettyConfigAction act)
      # td_ fil
      # td_ sc
      )

    toUl ids header = divClass "ignored-obs"
        ( h4_ header
        # ul_ (map (li_ . unId) ids)
        )

    ignoredIds, unknownIds :: [Id Observation]
    (ignoredIds, unknownIds) = ignoredObservations
        (configIgnored config)
        analysisIgnoredObservations

stanFooter = footer_
    ( divClass "container"
        ( divClass "row"
            ( span_ "This report was generated by "
            # a_A (A.href_ "https://github.com/kowainik/stan") "Stan — Haskell Static Analysis Tool."
            )
        # divClass "row"
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

divClass c = div_A (A.class_ c)
divIdClass i c = div_A (A.id_ i # A.class_ c)

divIdClassH h c rest = divIdClass (hToId h) c (h2_ h # rest)

hToId :: String -> String
hToId = intercalate "-" . map (map toLower) . List.words

nothingIfTrue :: Bool -> a -> Maybe a
nothingIfTrue p a = if p then Nothing else Just a
