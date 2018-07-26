-- | This module contains 'Warning' data type and functions to show warnings to user.

module Stan.Warning
       ( Warning (..)
       , showWarning
       , moduleOverall
       ) where

import SrcLoc (RealSrcSpan (..), srcSpanStartCol, srcSpanStartLine)

-- | Data type to represent possible warning after static analysis check.
data Warning
    -- | Usage of the partial functions from the @base@.
    = Partial String RealSrcSpan
    deriving (Show)

-- | By given source file content return the warning text.
showWarning :: String -> Warning -> String
showWarning source (Partial name src) = let (line, col) = srcSpanToNum src in unlines
    [ position line col ++ "Warning: Usage of the partial function '" ++ name ++ "'"
    , ""
    , codeOutput source line col (length name)
    , ""
    ]

-- | By given 'RealSrcSpan' returns the line and the column (starting from 1)
srcSpanToNum :: RealSrcSpan -> (Int, Int)
srcSpanToNum src = (srcSpanStartLine src, srcSpanStartCol src)

-- | Returns position in the following format: @(line:column)@.
position :: Int -> Int -> String
position l c = "(" ++ show l ++ ":" ++ show c ++ ") "

{- | Returns the line from the source code.

For example for given line 4 and column 15 and the hightlight length 4 it will
look like this:

@
  |
4 | main = print $ head ["Hello"]
  |                ^^^^
@
-}
codeOutput :: String -> Int -> Int -> Int -> String
codeOutput source l c nameLen = let n = length (show l) + 1 in unlines
    [ replicate n ' ' ++ "|"
    , show l ++ " | " ++ lines source !! (l - 1)
    , replicate n ' ' ++ "| " ++ replicate (c - 1) ' ' ++ replicate nameLen '^'
    ]

{- | Highlight message for the module with number of warnings information.

For example:

@
src/Hello.hs: Found 3 warning(s)
--------------------
@
-}
moduleOverall :: [Warning] -> String -> String
moduleOverall ws name = unlines
    [ name ++ ": Found " ++ show (length ws) ++ " warning(s)"
    , replicate 20 '-'
    ]
