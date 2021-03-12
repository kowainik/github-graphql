{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Submit HTTPS query to to GitHub GraphQL API.
-}

-- TODO: better module name
module GitHub.Query
    ( -- * Token
      GitHubToken (..)
    , getGitHubToken

      -- * API
      -- ** Queries
    , queryGitHub
    , queryRepositoryId
      -- ** Mutations
    , mutationGitHub
      -- ** Internals
    , callGitHubRaw
    ) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..), eitherDecode, encode, object, withObject, (.:), (.=))
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Text (Text)
import Network.HTTP.Client (RequestBody (RequestBodyLBS), httpLbs, method, parseRequest,
                            requestBody, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import Prolens (set)
import System.Environment (lookupEnv)
import Type.Reflection (Typeable, typeRep)

import GitHub.GraphQL (Mutation, Query, one)
import GitHub.Id (RepositoryId)
import GitHub.Render (renderTopMutation, renderTopQuery)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified GitHub.Lens as GH
import qualified GitHub.Repository as GH


{- | GitHub OAuth token.
-}
newtype GitHubToken = GitHubToken
    { unGitHubToken :: ByteString
    }

{- | Extract 'GitHubToken' from the given environment variable.

Typical usage:

@
maybeToken <- 'getGitHubToken' \"GITHUB_TOKEN\"
@

Returns 'Nothing' if the given environment variable name doesn't exist.
-}
getGitHubToken :: String -> IO (Maybe GitHubToken)
getGitHubToken varName = fmap encodeToken <$> lookupEnv varName
  where
    encodeToken :: String -> GitHubToken
    encodeToken = GitHubToken . Text.encodeUtf8 . Text.pack

{- | Call GitHub API with a token using 'Query' and return value that
has 'FromJSON' instance.
-}
queryGitHub
    :: forall a
    .  (Typeable a, FromJSON a)
    => GitHubToken  -- ^ Bearer token
    -> Query  -- ^ GraphQL query
    -> IO a
queryGitHub token = callGitHubRaw token . renderTopQuery

{- | Helper function to fetch 'RepositoryId'. It is often needed as
argument to other queries.

This function parses resulting JSON of the following shape:

@
{
  "data": {
    "repository": {
      "id": "MDEwOlJlcG9zaXRvcnkyOTA1MDA2MzI="
    }
  }
}
@
-}
queryRepositoryId
    :: GitHubToken  -- ^ Bearer token
    -> Text  -- ^ Owner
    -> Text  -- ^ Repository name
    -> IO RepositoryId
queryRepositoryId token owner repoName =
    fmap unRepositoryData
    $ queryGitHub @(RepositoryData RepositoryId) token
    $ GH.repositoryToAst repositoryIdQuery
  where
    repositoryIdQuery :: GH.Repository
    repositoryIdQuery = GH.repository
        ( GH.defRepositoryArgs
        & set GH.ownerL owner
        & set GH.nameL  repoName
        )
        $ one GH.RepositoryId

-- | Wrapper to parse values inside the "repository" object key.
newtype RepositoryData a = RepositoryData
    { unRepositoryData :: a
    }

instance (Typeable a, FromJSON a) => FromJSON (RepositoryData a) where
    parseJSON = withObject ("RepositoryData " ++ typeName @a) $ \o ->
        RepositoryData <$> (o .: "repository")

{- | Call GitHub API with a token using 'Mutation' and return value that
has 'FromJSON' instance.
-}
mutationGitHub
    :: forall a
    .  (Typeable a, FromJSON a)
    => GitHubToken  -- ^ Bearer token
    -> Mutation  -- ^ GraphQL mutation
    -> IO a
mutationGitHub token = callGitHubRaw token . renderTopMutation

{- | Call GitHub API with a token using raw text.
-}
callGitHubRaw
    :: forall a
    .  (Typeable a, FromJSON a)
    => GitHubToken  -- ^ Bearer token
    -> Text  -- ^ GraphQL query
    -> IO a
callGitHubRaw (GitHubToken token) text = do
    manager <- newTlsManager

    initialRequest <- parseRequest "https://api.github.com/graphql"
    let queryBody = object ["query" .= text]

    let request = initialRequest
            { method = "POST"
            , requestHeaders =
                  ("Authorization", "bearer " <> token)
                : ("User-Agent", "kowainik/github-graphql")
                : requestHeaders initialRequest
            , requestBody = RequestBodyLBS $ encode queryBody
            }

    -- calling the API
    response <- httpLbs request manager

    case statusCode $ responseStatus response of
        200 -> case eitherDecode @(GitHubDataResponse a) (responseBody response) of
            Left err ->
                throwIO $ userError $ "Error decoding JSON response: " <> err
            Right (GitHubDataResponse res) ->
                pure res
        code -> throwIO $ userError $ "Non-200 response code: " <> show code

{- | Local wrapper to parse objects inside the @data@ field. GraphQL
API returns JSON of the following shape.

@
{
    "data": { ... actual result ... }
}
@

This wrapper handles extra layer of @data@.
-}
newtype GitHubDataResponse a = GitHubDataResponse a

instance (Typeable a, FromJSON a) => FromJSON (GitHubDataResponse a) where
    parseJSON = withObject ("GitHubDataResponse " ++ typeName @a) $ \o -> do
        val <- o .: "data"
        pure $ GitHubDataResponse val

-- copy-pasted from @relude@
typeName :: forall a. Typeable a => String
typeName = show (typeRep @a)
