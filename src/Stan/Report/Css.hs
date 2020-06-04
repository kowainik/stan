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
             center, collapse, color, display, fontFamily, footer, main_, margin, padding, pre, px,
             sansSerif, solid, table, td, textAlign, th, tr, width, (**), (?))
import Clay.Color (Color, black, blue, cyan, green, hsl, magenta, orange, pink, red, white, yellow)


stanCss :: Css
stanCss = do
    body ? do
        width (100%)
        marginAll 0
        fontFamily [] [sansSerif]
        color darkGrey
    main_ ?
        margin (0%) (10%) (0%) (10%)
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
    (".observation" <> "#configurations" <> "#stan-info") ** (table <> tr <> td <> th) ? do
        border solid (px 1) darkGrey
        borderCollapse collapse
    ".cat" ? backgroundColor pink
    ".severityStyle" ? backgroundColor cyan
    ".severityPerformance" ? backgroundColor blue
    ".severityPotentialBug" ? backgroundColor magenta
    ".severityWarning" ? backgroundColor yellow
    ".severityError" ? backgroundColor red

    ".remove" ? color red
    ".include" ? color green
    ".exclude" ? color orange
    ".ignore" ? color yellow

    ".centre" ? textAlign center

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
