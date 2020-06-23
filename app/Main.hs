module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)

import Stan (run)


main :: IO ()
main = setLocaleEncoding utf8 >> run
