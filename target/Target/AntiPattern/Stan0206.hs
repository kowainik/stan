{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Target.AntiPattern.Stan0206 where


data RecordExample a = RecordExample
    { strictField :: !Int
    , lazyField   :: Int
    , lazyVar     :: a
    }

newtype NewtypeExample1 = NewtypeExample1 Bool
newtype NewtypeExample2 = NewtypeExample2
    { unWrap :: Int
    }

data PlainExample
    = Mk1
        !Int
        Int
    | Mk2 !Int
    | Mk3 Bool
