{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module provides the data type for required fields of GraphQL
queries. Some functions and subqueries require certain field. We track
these requirements in the type-level lists to tailor error messages
accordingly.

See the "GitHub.Lens" module for the typeclasses, responsible for
manipulating lists of fields via the @lens@-like API.
-}

module GitHub.RequiredField
    ( RequiredField (..)

      -- * Type families for error messages
    , DisplayFields
    , FieldNameAndLens
    ) where

import GHC.TypeLits (ErrorMessage (..))
import Type.Errors.Pretty (type (%))


{- | Type of all possible required fields.
-}
data RequiredField
    = FieldOwner  -- ^ owner
    | FieldName  -- ^ name
    | FieldTitle  -- ^ title
    | FieldLimit  -- ^ first or last
    | FieldStates  -- ^ states
    | FieldDirection  -- ^ direction
    | FieldField  -- ^ field
    | FieldRepositoryId  -- ^ repositoryId

{- Display all requires fields with their lens hints.
-}
type family DisplayFields (fields :: [RequiredField]) :: ErrorMessage where
    DisplayFields '[] = 'Text ""
    DisplayFields (x ': xs) = FieldNameAndLens x % DisplayFields xs

{- | Type family to display a single required fields with the
corresponding lens hint.
-}
type family FieldNameAndLens (field :: RequiredField) :: ErrorMessage where
    FieldNameAndLens 'FieldOwner =
        "    * owner"
      % "        Lens: ownerL from GitHub.Lens.OwnerL"
    FieldNameAndLens 'FieldName =
        "    * name"
      % "        Lens: nameL from GitHub.Lens.NameL"
    FieldNameAndLens 'FieldTitle =
        "    * title"
      % "        Lens: titleL from GitHub.Lens.TitleL"
    FieldNameAndLens 'FieldLimit =
        "    * last or first"
      % "        Lens: nameL from GitHub.Lens.LimitL"
    FieldNameAndLens 'FieldStates =
        "    * states"
      % "        Lens: nameL from GitHub.Lens.StatesL"
    FieldNameAndLens 'FieldDirection =
        "    * direction"
      % "        Lens: directionL from GitHub.Lens.DirectionL"
    FieldNameAndLens 'FieldField =
        "    * field"
      % "        Lens: fieldL from GitHub.Lens.FieldL"
    FieldNameAndLens 'FieldRepositoryId =
        "    * repositoryId"
      % "        Lens: repositoryIdL from GitHub.Lens.RepositoryIdL"
