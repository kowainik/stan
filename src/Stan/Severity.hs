{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module introduces 'Severity' data type for expressing how severe the
message is. Also, it contains useful functions to work with 'Severity'.
-}

module Stan.Severity
    ( Severity (..)

      -- * Pretty printing
    , severityColour
    , prettyShowSeverity
    ) where

import Colourista (blue, bold, cyan, formatWith, magenta, red, yellow)


{- | Severity level of the inspection.

 +---------------+-----------------------------------------------------+
 | Severity      | Example                                             |
 +===============+=====================================================+
 | 'Style'       | Missing @infix@, or type signature in @where@       |
 +---------------+-----------------------------------------------------+
 | 'Performance' | Usage of 'Data.Foldable.sum', 'Data.Foldable.foldl' |
 +---------------+-----------------------------------------------------+
 | 'PotentialBug'| Some common user errors: @[0 .. length xs]@         |
 +---------------+-----------------------------------------------------+
 | 'Warning'     | Partial functions, like 'GHC.List.head'             |
 +---------------+-----------------------------------------------------+
 | 'Error'       | Usage of 'undefined' in code                        |
 +---------------+-----------------------------------------------------+

-}
data Severity
    -- | Code style issues. Usually harmless.
    = Style
    -- | Serious defects that could cause slowness and space leaking.
    | Performance
    -- | Human errors in code.
    | PotentialBug
    -- | Potential runtime errors on some inputs.
    | Warning
    -- | Dangerous behaviour.
    | Error
    deriving stock (Show, Eq)

-- | Get the colour of the severity level.
severityColour :: Severity -> Text
severityColour = \case
    Style        -> cyan
    Performance  -> blue
    PotentialBug -> magenta
    Warning      -> yellow
    Error        -> red

-- | Show 'Severity' in a human-friendly format.
prettyShowSeverity :: Severity -> Text
prettyShowSeverity s = formatWith [severityColour s, bold] $ show s
