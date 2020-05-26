{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Target.AntiPattern where


stanLengthXs :: [a] -> [Int]
stanLengthXs xs = [0 .. length xs]

stanLengthXsMinus1 :: [a] -> [Int]
stanLengthXsMinus1 xs = [0 .. length xs - 1]
