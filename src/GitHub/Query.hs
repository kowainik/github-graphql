{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Submit HTTPS query to to GitHub GraphQL API.
-}

-- TODO: better module name
module GitHub.Query
    ( GitHubToken (..)
    , callGitHub
    ) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..), eitherDecode, encode, object, withObject, (.:), (.=))
import Data.ByteString (ByteString)
import Network.HTTP.Client (RequestBody (RequestBodyLBS), httpLbs, method, parseRequest,
                            requestBody, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import Type.Reflection (Typeable, typeRep)

import GitHub.GraphQL (Query)
import GitHub.Render (renderTopQuery)


{- | GitHub OAuth token.
-}
newtype GitHubToken = GitHubToken
    { unGitHubToken :: ByteString
    }


{- | Call GitHub API with a token
-}
callGitHub
    :: forall a
    .  (Typeable a, FromJSON a)
    => GitHubToken  -- ^ Bearer token
    -> Query  -- ^ GraphQL query
    -> IO a
callGitHub (GitHubToken token) query = do
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
        200 -> case eitherDecode @(Data a) (responseBody response) of
            Left err ->
                throwIO $ userError $ "Error decoding JSON response: " <> err
            Right (Data res) ->
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
newtype Data a = Data a

instance (Typeable a, FromJSON a) => FromJSON (Data a) where
    parseJSON = withObject (typeName @a) $ \o -> do
        val <- o .: "data"
        pure $ Data val

-- copy-pasted from @relude@
typeName :: forall a. Typeable a => String
typeName = show (typeRep @a)