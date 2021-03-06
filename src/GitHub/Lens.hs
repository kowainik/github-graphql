{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Typeclasses for providing @lens@-like API for creating GraphQL
queries.
-}

module GitHub.Lens
    ( -- * Typeclasses with lenses
      DirectionL (..)
    , FieldL (..)
    , LimitL (..)
    , NameL (..)
    , OrderL (..)
    , OwnerL (..)
    , RepositoryIdL (..)
    , StatesL (..)
    , TitleL (..)

      -- * Internals
    , Delete
    ) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Prolens (Lens, Lens')

import GitHub.GraphQL (IssueOrderField, OrderDirection)
import GitHub.Id (RepositoryId)
import {-# SOURCE #-} GitHub.Order (Order)
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
class StatesL (r :: [RequiredField] -> Type) (state :: Type) | r -> state where
    statesL :: Lens (r args) (r (Delete 'FieldStates args)) (NonEmpty state) (NonEmpty state)

{- | Typeclass for lenses that can change issue order field.
-}
class FieldL (r :: [RequiredField] -> Type) where
    fieldL :: Lens (r args) (r (Delete 'FieldField args)) IssueOrderField IssueOrderField

{- | Typeclass for lenses that can change order direction.
-}
class DirectionL (r :: [RequiredField] -> Type) where
    directionL :: Lens (r args) (r (Delete 'FieldDirection args)) OrderDirection OrderDirection

{- | Typeclass for lenses that can change title.
-}
class TitleL (r :: [RequiredField] -> Type) where
    titleL :: Lens (r args) (r (Delete 'FieldTitle args)) Text Text

{- | Typeclass for lenses that can change repository id.
-}
class RepositoryIdL (r :: [RequiredField] -> Type) where
    repositoryIdL :: Lens (r args) (r (Delete 'FieldRepositoryId args)) RepositoryId RepositoryId

{- | Typeclass for lenses that can change optional 'Order'.
-}
class OrderL (r :: [RequiredField] -> Type) where
    orderL :: Lens' (r args) (Maybe (Order '[]))

-- Internal helpers

{- | Type family to delete element from the type-level list.
-}
type family Delete (e :: k) (list :: [k]) :: [k] where
    Delete _ '[]       = '[]
    Delete x (x ': xs) = xs
    Delete x (y ': xs) = y ': Delete x xs
