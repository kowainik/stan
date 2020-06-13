{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Target.AntiPattern.Stan0212 where

import System.IO.Unsafe (unsafeDupablePerformIO, unsafeFixIO, unsafeInterleaveIO, unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)


stanUndefined :: a
stanUndefined = undefined

stanUnsafeCoerce :: String -> Int
stanUnsafeCoerce = unsafeCoerce

stanUnsafePerformIO :: String
stanUnsafePerformIO = unsafePerformIO getLine

stanUnsafeInterleaveIO :: IO ()
stanUnsafeInterleaveIO = unsafeInterleaveIO (pure ())

stanUnsafeDupablePerformIO :: String
stanUnsafeDupablePerformIO = unsafeDupablePerformIO getLine

stanUnsafeFixIO :: IO ()
stanUnsafeFixIO = unsafeFixIO pure
