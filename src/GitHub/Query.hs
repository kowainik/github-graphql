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
    , queryGitHub
    ) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..), eitherDecode, encode, object, withObject, (.:), (.=))
import Data.ByteString (ByteString)
import Network.HTTP.Client (RequestBody (RequestBodyLBS), httpLbs, method, parseRequest,
                            requestBody, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (lookupEnv)
import Type.Reflection (Typeable, typeRep)

import GitHub.GraphQL (Query)
import GitHub.Render (renderTopQuery)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


{- | GitHub OAuth token.
-}
newtype GitHubToken = GitHubToken
    { unGitHubToken :: ByteString
    }

{- | Extract 'GitHubToken' from the given environment variable.

Typical usage:

@
maybeToken <- 'getGitHubToken' "GITHUB_TOKEN"
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
queryGitHub (GitHubToken token) query = do
    manager <- newTlsManager

    initialRequest <- parseRequest "https://api.github.com/graphql"
    let queryBody = object ["query" .= renderTopQuery query]

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
