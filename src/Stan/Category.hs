{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

__Category__ — a type of 'Stan.Inspection.Inspection'.
-}

module Stan.Category
    ( -- * Data type
      Category (..)

      -- * Pretty printing
    , prettyShowCategory

      -- * Stan categories
    , stanCategories
    , antiPattern
    , infinite
    , list
    , partial
    , spaceLeak
    , syntax
    , unsafe
    ) where

import Colourista (formatWith, magentaBg)


-- | A type of the inspection.
newtype Category = Category
    { unCategory :: Text
    } deriving newtype (Show, Eq, Hashable)

-- | Show 'Category' in a human-friendly format.
prettyShowCategory :: Category -> Text
prettyShowCategory cat = formatWith [magentaBg] $ "#" <> unCategory cat

-- | @List@ category of Stan inspections.
list :: Category
list = Category "List"

-- | @Partial@ category of Stan inspections.
partial :: Category
partial = Category "Partial"

-- | @Infinite@ category of Stan inspections.
infinite :: Category
infinite = Category "Infinite"

-- | @AntiPattern@ category of Stan inspections.
antiPattern :: Category
antiPattern = Category "AntiPattern"

-- | @SpaceLeak@ category of Stan inspections.
spaceLeak :: Category
spaceLeak = Category "SpaceLeak"

{- | @Syntax@ category of Stan inspections. Usually used in
'Stan.Severity.Style' inspections.
-}
syntax :: Category
syntax = Category "Syntax"

-- | @Unsafe@ category of Stan inspections.
unsafe :: Category
unsafe = Category "Unsafe"

-- | The list of all available Stan 'Category's.
stanCategories :: [Category]
stanCategories =
    [ antiPattern
    , infinite
    , list
    , partial
    , spaceLeak
    , syntax
    , unsafe
    ]
