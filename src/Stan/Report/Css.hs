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

import Prelude hiding (rem, (**))

import Clay (Css, Percentage, Selector, Size, after, auto, backgroundColor, block, body, border,
             borderCollapse, borderTop, both, center, clear, collapse, color, content, display,
             displayNone, displayTable, element, em, float, floatLeft, fontFamily, fontSize, footer,
             height, html, left, lineHeight, main_, margin, marginBottom, marginLeft, marginRight,
             marginTop, maxWidth, minHeight, padding, position, pre, px, query, relative, rem,
             sansSerif, solid, stringContent, table, td, textAlign, th, top, tr, unitless, width,
             ( # ), (**), (?))
import Clay.Color (Color, black, blue, cyan, green, hsl, magenta, orange, pink, red, white, yellow)

import qualified Clay.Media as M
import qualified Data.List.NonEmpty as NE


stanCss :: Css
stanCss = do
    grid
    main_ ? marginAuto
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
        margin2 (2%) (10%)
        paddingAll 2
    ".inspection" ? do
        margin2 (2%) (0%)
        border solid (px 1) blue
    ".solutions" ? do
        margin2 (1%) (10%)
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

grid :: Css
grid = do
    (html <> body) ? do
        height (100%)
        width (100%)
        marginAll 0
        paddingAll 0
        left (0%)
        top (0%)
        fontFamily [] [sansSerif]
        fontSize (100%)
        color darkGrey
        lineHeight (unitless 1.5)
    ".centre" ? (textAlign center) -- >> marginAuto)
    ".container" ? (width (90%) >> marginAuto)
    ".row" ? (position relative >> width (100%))
    ".row [class^='col']" ? do
        float floatLeft
        margin2 (rem 0.5) (2%)
        minHeight (rem 0.125)
    sconcat colClasses ? width (96%)
    colsGrid colClassesSm
    ".row" # after ? do
        content (stringContent "")
        display displayTable
        clear both
    ".hidden-sm" ? display displayNone
    mediaQuery 33.75 $ ".container" ? width (80%)
    mediaQuery 45 $ colsGrid colClasses
    mediaQuery 60 $ ".container" ? (width (75%) >> maxWidth (rem 60))

  where
    cols :: NonEmpty Text
    cols = fmap ((".col-" <>) . show) $ (1 :: Int) :| [2..12]

    colClasses, colClassesSm :: NonEmpty Selector
    colClasses   = fmap element cols
    colClassesSm = fmap (element . (<> "-sm")) cols

    colsGrid :: NonEmpty Selector -> Css
    colsGrid classes = sequence_ $ NE.map (\(cl, p) -> cl ? width (p %)) $ NE.zip classes w

    w :: NonEmpty Rational
    w = 4.33 :| [12.66, 21, 29.33, 37.66, 46, 54.33, 62.66, 71, 79.33, 87.66, 96]

    mediaQuery :: Double -> Css -> Css
    mediaQuery x = query M.screen [M.minWidth (em x)]

marginAuto :: Css
marginAuto = marginLeft auto >> marginRight auto

marginAll :: Size Percentage -> Css
marginAll x = margin x x x x

margin2 :: Size a -> Size b -> Css
margin2 x y = marginTopBottom x >> marginLeftRight y

marginTopBottom :: Size a -> Css
marginTopBottom x = marginTop x >> marginBottom x

marginLeftRight :: Size a -> Css
marginLeftRight x = marginLeft x >> marginRight x

paddingAll :: Size Percentage -> Css
paddingAll x = padding x x x x

(%) :: Rational -> Size Percentage
(%) = fromRational

lightGrey, darkGrey :: Color
lightGrey = hsl 0 0 74
darkGrey = hsl 0 0 38
