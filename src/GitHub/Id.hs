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

      -- * Different IDs
    , MilestoneId
    , RepositoryId
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text (Text)


{- | ID of different entities. Uses phantom type variables to
represent different types of IDs.
-}
newtype Id (idType :: IdType) = Id
   { unId :: Text
   } deriving stock (Show)
     deriving newtype (Eq, Ord)

instance FromJSON (Id idType) where
    parseJSON = withObject "Id" $ \o -> Id <$> (o .: "id")

data IdType
    = IDRepository
    | IDMilestone

type RepositoryId = Id 'IDRepository
type MilestoneId = Id 'IDMilestone
