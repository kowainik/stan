{-# LANGUAGE PostfixOperators #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Css to be used in the generated HTML in the report.
-}
module Stan.Report.Css
    ( stanCss
    ) where

import Prelude hiding ((**))

import Clay (Css, Percentage, Size, backgroundColor, block, body, border, borderCollapse, borderTop,
             center, collapse, color, display, fontFamily, footer, margin, padding, pre, px,
             sansSerif, solid, table, td, textAlign, tr, width, (**), (?))
import Clay.Color (Color, black, blue, cyan, hsl, magenta, pink, red, white, yellow)


stanCss :: Css
stanCss = do
    body ? do
        marginAll 0
        fontFamily [] [sansSerif]
        color darkGrey
    footer ? do
        display block
        textAlign center
        width (100%)
        color white
        backgroundColor lightGrey
        borderTop solid (px 8) darkGrey
    pre ? do
        backgroundColor black
        color white
        margin (2%) (10%) (2%) (10%)
        paddingAll 2
    ".inspection" ? do
        margin (2%) (0%) (2%) (0%)
        border solid (px 1) blue
    ".solutions" ? do
        margin (1%) (10%) (1%) (10%)
        paddingAll 1
        backgroundColor lightGrey
        border solid (px 2) darkGrey
    ".observation" ** (table <> tr <> td) ? do
        border solid (px 1) darkGrey
        borderCollapse collapse
    ".cat" ? backgroundColor pink
    ".severityStyle" ? backgroundColor cyan
    ".severityPerformance" ? backgroundColor blue
    ".severityPotentialBug" ? backgroundColor magenta
    ".severityWarning" ? backgroundColor yellow
    ".severityError" ? backgroundColor red

  where
    marginAll :: Size Percentage -> Css
    marginAll x = margin x x x x

    paddingAll :: Size Percentage -> Css
    paddingAll x = padding x x x x

    (%) :: Integer -> Size Percentage
    (%) = fromInteger

    lightGrey, darkGrey :: Color
    lightGrey = hsl 0 0 74
    darkGrey = hsl 0 0 38
