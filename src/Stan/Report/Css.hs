module Stan.Report.Css
    ( stanCss
    ) where

import Clay (Css, (?))

import qualified Clay as Css


stanCss :: Css
stanCss = do
    Css.body ? do
        Css.margin zero zero zero zero
        Css.fontFamily [] [Css.sansSerif]
        Css.color darkGrey
    Css.footer ? do
        Css.display Css.block
        Css.textAlign Css.center
        Css.width (100 :: Css.Size Css.Percentage)
        Css.color Css.white
        Css.backgroundColor lightGrey
        Css.borderTop Css.solid (Css.px 8) darkGrey
  where
    zero :: Css.Size Css.Percentage
    zero = 0

    lightGrey = Css.hsl 0 0 74
    darkGrey = Css.hsl 0 0 38
