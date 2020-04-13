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
    , partial
    ) where

import Colourista (formatWith, magentaBg)


-- | A type of the inspection.
newtype Category = Category
    { unCategory :: Text
    } deriving newtype (Show)

-- | Show 'Category' in a human-friendly format.
prettyShowCategory :: Category -> Text
prettyShowCategory cat = formatWith [magentaBg] $ "#" <> unCategory cat

-- | @Partial@ category of Stan inspections.
partial :: Category
partial = Category "Partial"

-- | The list of all available Stan 'Category's.
stanCategories :: [Category]
stanCategories =
    [ partial
    ]
