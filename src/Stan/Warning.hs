-- | This module contains 'Warning' data type and functions to show warnings to user.

module Stan.Warning
       ( Warning (..)
       , showWarning
       ) where

import SrcLoc (SrcSpan (..), srcSpanStartCol, srcSpanStartLine)

-- | Data type to represent possible warning after static analysis check.
data Warning
    -- | Usage of the partial functions from the @base@.
    = Partial String SrcSpan
    deriving (Show)

-- | By given source file content return the warning text.
showWarning :: String -> Warning -> String
showWarning source (Partial name src) = case srcSpanToNum src of
    Nothing -> ""
    Just (line, col) -> unlines
        [ "Warning: Usage of the partial function ''" ++ name ++ "'"
        , (lines source) !! line
        , replicate col ' ' ++ replicate (length name) '^'
        , ""
        ]

-- | By given 'SrcSpan' returns the line and the column (starting from 0) if
-- possible.
srcSpanToNum :: SrcSpan -> Maybe (Int, Int)
srcSpanToNum (RealSrcSpan src) = Just (srcSpanStartLine src - 1, srcSpanStartCol src - 1)
srcSpanToNum (UnhelpfulSpan _) = Nothing
