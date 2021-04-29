{-# LANGUAGE DataKinds #-}

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
    , queryMilestoneId
      -- ** Mutations
    , mutationGitHub
      -- ** Internals
    , callGitHubRaw
    ) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON, eitherDecode, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Text (Text)
import Network.HTTP.Client (RequestBody (RequestBodyLBS), httpLbs, method, parseRequest,
                            requestBody, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import Prolens (set)
import System.Environment (lookupEnv)

import GitHub.Common (one)
import GitHub.GraphQL (Mutation, Query)
import GitHub.Id (MilestoneId, RepositoryId)
import GitHub.Json (Nested (..))
import GitHub.Render (renderTopMutation, renderTopQuery)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified GitHub.Lens as GH
import qualified GitHub.Milestone as GH
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
    .  (FromJSON a)
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
    fmap (unNested @'[ "repository" ])
    $ queryGitHub token
    $ GH.repositoryToAst repositoryIdQuery
  where
    repositoryIdQuery :: GH.Repository
    repositoryIdQuery = GH.repository
        ( GH.defRepositoryArgs
        & set GH.ownerL owner
        & set GH.nameL  repoName
        )
        $ one GH.RepositoryId

{- | Helper function to fetch 'MilestoneId'. It is often needed as
argument to other queries.

This function parses resulting JSON of the following shape:

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
-}
queryMilestoneId
    :: GitHubToken  -- ^ Bearer token
    -> Text  -- ^ Owner
    -> Text  -- ^ Repository name
    -> Int   -- ^ Milestone number
    -> IO MilestoneId
queryMilestoneId token owner repoName milestoneNumber =
    fmap (unNested @'[ "repository", "milestone" ])
    $ queryGitHub token
    $ GH.repositoryToAst milestoneIdQuery
  where
    milestoneIdQuery :: GH.Repository
    milestoneIdQuery = GH.repository
        ( GH.defRepositoryArgs
        & set GH.ownerL owner
        & set GH.nameL  repoName
        )
        $ one
        $ GH.milestone
            ( GH.defMilestoneArgs
            & set GH.numberL milestoneNumber
            )
            (one $ GH.MilestoneId)

{- | Call GitHub API with a token using 'Mutation' and return value that
has 'FromJSON' instance.
-}
mutationGitHub
    :: forall a
    .  (FromJSON a)
    => GitHubToken  -- ^ Bearer token
    -> Mutation  -- ^ GraphQL mutation
    -> IO a
mutationGitHub token = callGitHubRaw token . renderTopMutation

{- | Call GitHub API with a token using raw text.
-}
callGitHubRaw
    :: forall a
    .  FromJSON a
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
        200 -> case eitherDecode @(Nested '[ "data" ] a) (responseBody response) of
            Left err ->
                throwIO $ userError $ "Error decoding JSON response: " <> err
            Right (Nested res) ->
                pure res
        code -> throwIO $ userError $ "Non-200 response code: " <> show code
