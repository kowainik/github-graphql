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
    , GitHubError (..)
    , callGitHubRaw
    , unNest
    ) where

import Data.Aeson (FromJSON, eitherDecode, encode, object, (.=))
import Data.Bifunctor (second)
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
import GitHub.Json (GitHubErrorDetails, Nested (..))
import GitHub.Render (renderTopMutation, renderTopQuery)

import qualified Data.ByteString.Lazy as LBS
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
    -> IO (Either GitHubError a)
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
    -> IO (Either GitHubError RepositoryId)
queryRepositoryId token owner repoName =
    unNest @'[ "repository" ]
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
    -> IO (Either GitHubError MilestoneId)
queryMilestoneId token owner repoName milestoneNumber =
    unNest @'[ "repository", "milestone" ]
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
    -> IO (Either GitHubError a)
mutationGitHub token = callGitHubRaw token . renderTopMutation

{- | Call GitHub API with a token using raw text.
-}
callGitHubRaw
    :: forall a
    .  FromJSON a
    => GitHubToken  -- ^ Bearer token
    -> Text  -- ^ GraphQL query
    -> IO (Either GitHubError a)
callGitHubRaw (GitHubToken token) query = do
    manager <- newTlsManager

    initialRequest <- parseRequest "https://api.github.com/graphql"
    let queryBody = object ["query" .= query]

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
    let responseCode = statusCode $ responseStatus response
    let jsonBody     = responseBody response

    let mkError :: Maybe String -> [GitHubErrorDetails] -> GitHubError
        mkError gitHubErrorJsonParsing gitHubErrorDetails = GitHubError
            { gitHubErrorResponseCode = responseCode
            , gitHubErrorQuery        = query
            , gitHubErrorJsonBody     = jsonBody
            , ..
            }

    pure $ case responseCode of
        200 -> case decodeResult jsonBody of
            Right res   -> Right res
            Left resErr -> case decodeErrors jsonBody of
                Right errs      -> Left $ mkError (Just resErr) errs
                Left detailsErr -> Left $ mkError (Just detailsErr) []

        _code -> Left $ mkError Nothing []
  where
    decodeResult :: LBS.ByteString -> Either String a
    decodeResult = fmap unNested . eitherDecode @(Nested '[ "data" ] a)

    decodeErrors :: LBS.ByteString -> Either String [GitHubErrorDetails]
    decodeErrors = fmap unNested . eitherDecode @(Nested '[ "errors" ] [GitHubErrorDetails])

data GitHubError = GitHubError
    { gitHubErrorResponseCode :: Int
    , gitHubErrorQuery        :: Text
    , gitHubErrorJsonBody     :: LBS.ByteString
    , gitHubErrorJsonParsing  :: Maybe String
    , gitHubErrorDetails      :: [GitHubErrorDetails]
    } deriving stock (Show, Eq)

{- | Helper function to work with GraphQL Queries that return 'Either'.
-}
unNest
    :: forall keys a
    .  IO (Either GitHubError (Nested keys a))
    -> IO (Either GitHubError a)
unNest = fmap (second unNested)
