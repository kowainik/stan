{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ExistentialQuantification #-}
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

data ConstraintExample
    = forall a .
        Constr a
    | forall a b .
        Constr2 a
          !b
    | forall a . (Num a) =>
        Constr3 a
    | forall a b . (Num a, Ord b) =>
        Constr4 a
          !b

data ConstraintRecord = forall a . (Num a) =>
    ConstraintRecord
    { crInt :: Int
    , crNum :: a
    }
