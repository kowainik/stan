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

import Prelude hiding (div, rem, (&), (**))

import Clay hiding (brown, cols, grid)

import qualified Clay.Media as M
import qualified Data.List.NonEmpty as NE


stanCss :: Css
stanCss = do
    grid
    main_ ? marginAuto
    nav ? do
        backgroundColor darkGrey
        color yellow
        padding2 (1%) (0%)
    -- ".nav-item" |> a ? do
    a ? do
        textDecoration none
        color yellow
        "@href" & do
           textDecoration underline
           ":hover" & fontWeight bold
    footer <> header ? do
        display block
        textAlign center
        width (100%)
        maxWidth (100%)
        backgroundColor lightGrey
        borderTop solid (px 15) darkGrey
    footer |> ".container" ? marginTopBottom (px 20)
    pre ? do
        backgroundColor brown
        color white
        margin2 (2%) (10%)
        paddingAll 2
        overflowX auto
    ".solutions" ? do
        margin2 (1%) (10%)
        paddingAll 1
        backgroundColor lightGrey
        border solid (px 2) darkGrey
    table ? width (100%)
    td <> th ? padding2 nil (px 8)
    (".observation" <> "#configurations" <> "#stan-info" <> "#severity") ** (table <> tr <> td <> th) ? do
        border solid (px 1) darkGrey
        borderCollapse collapse
    blockquote ? do
        paddingLeft (2%)
        borderLeft solid (px 4) darkGrey
        boxShadow $ one $ bsColor lightGrey $ shadow (px (-4)) 0

    -- Categories
    ".cats" ? (listStyle none none none >> overflow hidden >> paddingAll 0)
    ".cats" |> li ? float floatLeft
    ".cat" ? do
        backgroundColor pink
        borderRadius (px 3) (px 0) (px 0) (px 3)
        display inlineBlock
        padding (px 0) (px 20) (px 0) (px 23)
        textDecoration none
        position relative
        transitionProperty "color"
        transitionDuration (sec 0.2)
    ".cat" # before ? do
        backgroundColor white
        borderRadius (px 10) (px 10) (px 10) (px 10)
        boxShadow $ one $ bsInset $ bsColor (rgba 0 0 0 0.25) $
            shadow (px 0) (px 1)
        content (stringContent "")
        height (px 6)
        left (px 10)
        position absolute
        width (px 6)
        top (px 10)
    ".cat" # after ? do
        backgroundColor veryLightGrey
        borderBottom solid (px 13) transparent
        borderLeft   solid (px 10) pink
        borderTop    solid (px 13) transparent
        content (stringContent "")
        position absolute
        right (0%) >> top (0%)

    ".severity" ? do
        display inlineBlock
        padding (px 1) 0 0 0
        border solid (px 1) darkGrey
        borderRadius (px 4) (px 4) (px 4) (px 4)
        lineHeight (unitless 1)
    ".severityText" ? padding2 (px 0) (px 15)
    ".severityStyle"        ? severityCss cyan
    ".severityPerformance"  ? severityCss blue
    ".severityPotentialBug" ? severityCss magenta
    ".severityWarning"      ? severityCss yellow
    ".severityError"        ? severityCss red

    ".remove"  ? configActionsCss red
    ".include" ? configActionsCss green
    ".exclude" ? configActionsCss yellow
    ".ignore"  ? configActionsCss orange

    collapsible
  where
    configActionsCss :: Color -> Css
    configActionsCss c = color black >> backgroundColor (setA 0.5 c)

    severityCss :: Color -> Css
    severityCss c = do
        padding2 (px 0) (px 15)
        height (100%)
        backgroundColor c
        backgroundClip $ boxClip paddingBox
        borderRadius (px 4) (px 0) (px 0) (px 4)
        borderRight solid (px 1) darkGrey

collapsible :: Css
collapsible = do
    ".collapsible" ? do
        width (100%)
        fontSize (rem 1.125)
        important (marginLeft (0%) >> marginBottom (0%) >> marginRight (0%))
        backgroundColor darkGrey
        color white
        cursor pointer
        textAlign (alignSide sideLeft)

    ".active" <> (".collapsible" # hover) ? do
        backgroundColor lightGrey

    ".collapsible" # after ? do
        content (stringContent "\\002B")
        color white
        fontWeight bold
        float floatRight
        marginLeft (px 5)

    ".active" # after ? content (stringContent "\\2212")

    ".content" ? do
        maxHeight nil
        overflow hidden
        transitionProperty "max-height"
        transitionTimingFunction easeOut
        transitionDuration (sec 0.2)
        backgroundColor veryLightGrey
    ".content" |> div ?
        padding 0 0 0 (px 18)

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
    h1 ? fontSize (rem 2.5)
    h2 ? fontSize (rem 2)
    h3 ? fontSize (rem 1.375)
    h4 ? fontSize (rem 1.125)
    h5 ? fontSize (rem 1)
    h6 ? fontSize (rem 0.875)
    p ? (fontSize (rem 1.125) >> fontWeight (weight 200) >> lineHeight (unitless 1.8))
    ".centre" ? (textAlign center >> marginAuto)
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
    colsGrid classes = sequence_ $ NE.zipWith (\cl per -> cl ? width (per %)) classes w

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

padding2 :: Size a -> Size a -> Css
padding2 x y = padding x y x y

(%) :: Rational -> Size Percentage
(%) = fromRational

lightGrey, darkGrey, veryLightGrey, brown :: Color
lightGrey = rgb 189 189 189
darkGrey = rgb 97 97 97
veryLightGrey = rgb 241 241 241
brown = rgb 78 52 46
