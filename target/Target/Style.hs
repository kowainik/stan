{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Target.Style where


(???) :: Int -> Int -> Int
(???) = (+)

infixl 7 ***
(***) :: Int -> Int -> Int
(***) = (*)
