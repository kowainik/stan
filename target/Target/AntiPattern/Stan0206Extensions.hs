{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

{-# LANGUAGE StrictData #-}

module Target.AntiPattern.Stan0206Extensions where


data Record a = Record
    { field1 :: Int
    , field2 :: ~a
    }

data User = User Int String
