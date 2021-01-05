{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Utilities to provide general @lens@-like API for creating GraphQL
queries.
-}

module GitHub.Lens
    ( HasOwnerName (..)
    , HasLimit (..)
    , HasStates (..)

      -- * Parameter types
    , ArgsType (..)

      -- * Internas
    , Delete
    ) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Prolens (Lens)

import GitHub.GraphQL (State)


{- | Type of fields for arguments.
-}
data ArgsType
    = ArgsOwner
    | ArgsName
    | ArgsLimit
    | ArgsStates

{- | Typeclass for lenses that can change owner and name
-}
class HasOwnerName (r :: [ArgsType] -> Type) where
    ownerL :: Lens (r args) (r (Delete 'ArgsOwner args)) Text Text
    nameL  :: Lens (r args) (r (Delete 'ArgsName args)) Text Text

{- | Typeclass for lenses that can change the limit of elements.
-}
class HasLimit (r :: [ArgsType] -> Type) where
    lastL  :: Lens (r args) (r (Delete 'ArgsLimit args)) Int Int
    -- firstL :: Lens (r args) (r (Delete 'ArgsLimit args)) Int Int

{- | Typeclass for lenses that can change states.
-}
class HasStates (r :: [ArgsType] -> Type) where
    statesL :: Lens (r args) (r (Delete 'ArgsStates args)) (NonEmpty State) (NonEmpty State)


-- Internal helpers

{- | Type family to delete element from the type-level list.
-}
type family Delete (e :: k) (list :: [k]) :: [k] where
    Delete _ '[]       = '[]
    Delete x (x ': xs) = xs
    Delete x (y ': xs) = y ': Delete x xs
