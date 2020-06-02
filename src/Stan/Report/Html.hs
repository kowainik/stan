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
import Html (body_, div_A, doctype_, footer_, h1_, h2_, head_, header_, html_, main_, meta_A, p_,
             pre_, style_, title_, ( # ))

import Stan.Report.Css (stanCss)

import qualified Data.List as List (words)
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
    # divIdClassH "Observations" "" (pre_ an)
    # divIdClassH "Configurations" "" (p_ "Later")
    # divIdClassH "Summary" "" (p_ "Later")
    # divIdClassH "Inspections" "" (p_ "Later")
    )

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
