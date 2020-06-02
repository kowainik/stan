{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Css to be used in the generated HTML in the report.
-}
module Stan.Report.Css
    ( stanCss
    ) where

import Clay (Color, Css, Percentage, Size, backgroundColor, block, body, borderTop, center, color,
             display, fontFamily, footer, hsl, margin, px, sansSerif, solid, textAlign, white,
             width, (?))


stanCss :: Css
stanCss = do
    body ? do
        margin zero zero zero zero
        fontFamily [] [sansSerif]
        color darkGrey
    footer ? do
        display block
        textAlign center
        width (100 :: Size Percentage)
        color white
        backgroundColor lightGrey
        borderTop solid (px 8) darkGrey
  where
    zero :: Size Percentage
    zero = 0

    lightGrey, darkGrey :: Color
    lightGrey = hsl 0 0 74
    darkGrey = hsl 0 0 38
