{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Utilities to parse JSON returned by the API easier.
-}

module GitHub.Json
    ( -- * Nested parsing
      Nested (..)

      -- * Error messages
    , GitHubErrorDetails (..)
    , ErrorLocation (..)
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import qualified Data.Text as Text


{- | GitHub GraphQL API usually returns very nested JSON objects:

@
{
  "data": {
    "repository": {
      "milestone": {
        "id": "MDk6TWlsZXN0b25lNDY3NjQwNQ=="
      }
    }
  }
}
@

Since @aeson@ doesn't provide easy ways to parse nested objects and we
don't want to depend on the @lens-aeson@ package, we provide this
newtype wrapper.

To parse the above JSON into a single value of type 'MilestoneId', use one of
the values of one the following types:

@
Nested '[ "data", "repository", "milestone", "id" ] Text
Nested '[ "data", "repository", "milestone" ] MilestoneId
@

However, in real-life when using queries of this library, you don't
need to specify @"data"@ as it handled internally.

The code looks like this:

@
unNested @'[ "repository", "milestone" ] @MilestoneId <$> callGitHubRaw
@
-}
newtype Nested (keys :: [Symbol]) a = Nested
    { unNested :: a
    }

instance (FromJSON a) => FromJSON (Nested '[] a)
  where
    parseJSON = fmap Nested . parseJSON @a

instance (FromJSON (Nested keys a), KnownSymbol key) => FromJSON (Nested (key ': keys) a)
  where
    parseJSON = withObject ("Nested " <> keyStr) $ \o -> do
        nested <- o .: key
        Nested . unNested <$> parseJSON @(Nested keys a) nested
      where
        keyStr :: String
        keyStr = symbolVal (Proxy @key)

        key :: Text
        key = Text.pack keyStr

data GitHubErrorDetails = GitHubErrorDetails
    { gitHubErrorDetailsMessage   :: Text
    , gitHubErrorDetailsLocations :: [ErrorLocation]
    } deriving stock (Show, Eq)

instance FromJSON GitHubErrorDetails
  where
    parseJSON = withObject "GitHubErrorDetails" $ \o -> do
        gitHubErrorDetailsMessage   <- o .: "message"
        gitHubErrorDetailsLocations <- o .: "locations"
        pure GitHubErrorDetails{..}

data ErrorLocation = ErrorLocation
    { errorLocationLine   :: Int
    , errorLocationColumn :: Int
    } deriving stock (Show, Eq)

instance FromJSON ErrorLocation
  where
    parseJSON = withObject "ErrorLocation" $ \o -> do
        errorLocationLine   <- o .: "line"
        errorLocationColumn <- o .: "column"
        pure ErrorLocation{..}
