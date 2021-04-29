{-# LANGUAGE DataKinds #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

ID of different entities.

* https://docs.github.com/en/graphql/reference/scalars#id
-}


module GitHub.Id
    ( -- * Main ID type
      Id (..)
    , IdType (..)
    , castId

      -- * Different IDs
    , AnyId
    , IssueId
    , MilestoneId
    , RepositoryId
    , UserId
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text (Text)
import Type.Reflection (Typeable)

import GitHub.Common (typeName)


{- | ID of different entities. Uses phantom type variables to
represent different types of IDs.
-}
newtype Id (idType :: IdType) = Id
   { unId :: Text
   } deriving stock (Show)
     deriving newtype (Eq, Ord)

instance Typeable idType => FromJSON (Id idType) where
    parseJSON = withObject ("Id" <> typeName @idType) $ \o -> Id <$> (o .: "id")

data IdType
    = IDAny
    | IDIssue
    | IDMilestone
    | IDRepository
    | IDUser

type AnyId = Id 'IDAny
type IssueId = Id 'IDMilestone
type MilestoneId = Id 'IDMilestone
type RepositoryId = Id 'IDRepository
type UserId = Id 'IDUser

castId
    :: forall (to :: IdType) (from :: IdType)
    .  Id from
    -> Id to
castId (Id i) = Id i
