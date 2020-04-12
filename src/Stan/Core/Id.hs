{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Safe 'Id' representation.
-}

module Stan.Core.Id
    ( Id (..)
    , AnyId
    , castId
    ) where

import Data.Type.Equality (type (==))


{- | A wrapper around the textual value to safely represent IDs for different
structures by using a phantom parameter.
-}
newtype Id a = Id
    { unId :: Text
    } deriving stock (Show)

{- | A type alias for the situations when we don't care about the parameter of
'Id' but don't want to deal with type variables.
-}
type AnyId = Id ()

{- | Unsafe cast of 'Id'. Implementation uses smart trick to enforce usage
always with @TypeApplications@.
-}
castId
    :: forall to from to'
    .  ((to == to') ~ 'True)
    => Id from
    -> Id to'
castId (Id a) = Id a
