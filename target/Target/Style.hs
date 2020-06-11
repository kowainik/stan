{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Target.Style where


(???) :: Int -> Int -> Int
(???) = (+)

infixl 7 ***
(***) :: Int -> Int -> Int
(***) = (*)

quad :: Int -> (Int, Int, Int, Int)
quad x = (x, x, x, x)

triple :: Int -> (Int, Int, Int)
triple y = (y, y, y)

data TupleRecord = TupleRecord
    { quadruple :: (Int, Int, Int, Int)
    }
