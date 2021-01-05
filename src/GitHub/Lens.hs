{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Typeclasses for providing @lens@-like API for creating GraphQL
queries.
-}

module GitHub.Lens
    ( -- * Typeclasses with lenses
      LimitL (..)
    , NameL (..)
    , OwnerL (..)
    , StatesL (..)

      -- * Internals
    , Delete
    ) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Prolens (Lens)

import GitHub.GraphQL (State)
import GitHub.RequiredField (RequiredField (..))


{- | Typeclass for lenses that can change the limit of elements.
-}
class LimitL (r :: [RequiredField] -> Type) where
    lastL  :: Lens (r args) (r (Delete 'FieldLimit args)) Int Int
    -- firstL :: Lens (r args) (r (Delete 'ArgsLimit args)) Int Int

{- | Typeclass for lenses that can change owner.
-}
class OwnerL (r :: [RequiredField] -> Type) where
    ownerL :: Lens (r args) (r (Delete 'FieldOwner args)) Text Text

{- | Typeclass for lenses that can change name.
-}
class NameL (r :: [RequiredField] -> Type) where
    nameL  :: Lens (r args) (r (Delete 'FieldName args)) Text Text

{- | Typeclass for lenses that can change states.
-}
class StatesL (r :: [RequiredField] -> Type) where
    statesL :: Lens (r args) (r (Delete 'FieldStates args)) (NonEmpty State) (NonEmpty State)


-- Internal helpers

{- | Type family to delete element from the type-level list.
-}
type family Delete (e :: k) (list :: [k]) :: [k] where
    Delete _ '[]       = '[]
    Delete x (x ': xs) = xs
    Delete x (y ': xs) = y ': Delete x xs
