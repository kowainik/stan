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

import Clay (render)
import Data.Char (toLower)
import Html (a_A, body_, div_A, doctype_, footer_, h1_, h2_, h3_, head_, header_, html_, li_, main_,
             meta_A, p_, pre_, strong_, style_, table_, td_, title_, tr_, ul_, ( # ))

import Stan.Analysis (Analysis (..))
import Stan.Category (Category (..))
import Stan.Core.Id (Id (..))
import Stan.Core.ModuleName (ModuleName (..))
import Stan.FileInfo (FileInfo (..))
import Stan.Inspection (Inspection (..))
import Stan.Inspection.All (getInspectionById)
import Stan.Observation (Observation (..), prettyObservationSource)
import Stan.Report.Css (stanCss)

import qualified Data.List as List (words)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Html.Attribute as A


stanHtml an = doctype_ # html_ (stanHead # stanBody)
  where
    stanBody = body_ (stanHeader # stanMain an # stanFooter)

stanHeader = header_ (h1_ "Stan Report")

stanMain an = main_
    ( divIdClass "general-info" ""
      ( divIdClassH "Project Info" "" ()
      # divIdClassH "Stan Info" "" ()
      # divIdClassH "Analysis Info" "" ()
      )
    # divIdClassH "Graphs" "" (p_ "Maybe later")
    # divIdClassH "Observations" "" (stanObservations an)
    # divIdClassH "Configurations" "" (p_ "Later")
    # divIdClassH "Summary" "" (p_ "Later")
    # divIdClassH "Inspections" "" (p_ "Later")
    )

stanObservations Analysis{..} =
    map stanPerFile
    $ filter (not . null . fileInfoObservations)
    $ Map.elems analysisFileMap

stanPerFile FileInfo{..} = divIdClass "" "" ( h3_ fileInfoPath # ul_
    ( li_ ("Module: " # unModuleName fileInfoModuleName)
    # li_ ("Lines of Code:" <> show fileInfoLoc)
    # li_ (divClass "observations"
      ( strong_ "Observations" # map stanObservation (toList fileInfoObservations)))
    )
    )

stanObservation o@Observation{..} = divIdClass (unId observationId) "observation"
    ( general
    # pre_ (unlines $ prettyObservationSource False o)
    # nothingIfTrue (null solutions) (divClass "solutions"
        ( p_ "Possible solution"
        # ul_ (map li_ solutions)
        ))
    )
  where
    general = divClass "observation-general" $ table_
        ( tr_ (td_ "ID " # td_ (strong_ $ unId observationId))
        # tr_ (td_ "Severity" # td_ (show @Text $ inspectionSeverity inspection))
        # tr_ (td_ "Description" # td_ (inspectionDescription inspection))
        # tr_ (td_ "Inspection ID" # td_ (a_A (A.href_ $ toText "#" <> insId) insId))
        # tr_ (td_ "Category" # td_ (toText "#" <> T.intercalate (toText " #")
                  (map unCategory $ toList $ inspectionCategory inspection))
              )
        # tr_ (td_ "File" # td_ observationFile)
        )

    inspection :: Inspection
    inspection = getInspectionById observationInspectionId

    insId :: Text
    insId = unId $ observationInspectionId

    solutions :: [Text]
    solutions = inspectionSolution inspection

divClass c = div_A (A.class_ c)
divIdClass i c = div_A (A.id_ i # A.class_ c)

divIdClassH h c rest = divIdClass i c (h2_ h # rest)
  where
    i :: String
    i = intercalate "-" $ map (map toLower) $ List.words h

stanFooter = footer_ ("(c) Kowainik 2020")

stanHead = head_
    ( meta_A (A.httpEquiv_ "Content-Type" # A.content_ "text/html; charset=UTF-8")
    # meta_A (A.httpEquiv_ "X-UA-Compatible" # A.content_ "IE=Edge")
    # nameContent "viewport" "width=device-width, initial-scale=1.0"
    # nameContent "description" "Stan Report"
    # nameContent "keywords" "Haskell, Static Analysis"
    # nameContent "author" "Kowainik"
    # title_ "Stan Report"

    # style_ (render stanCss)
    )
  where
    nameContent x y = meta_A (A.name_ x # A.content_ y)

nothingIfTrue :: Bool -> a -> Maybe a
nothingIfTrue p a = if p then Nothing else Just a
