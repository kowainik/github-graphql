{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

GraphQL AST renderer.
-}

module GitHub.Render
    ( renderTopQuery
    , renderQuery
    , renderQueryNode
    , renderNodeName
    , renderQueryParam
    , renderParamName
    , renderParamValue
    , renderState
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (stimes)
import Data.Text (Text)

import GitHub.GraphQL (NodeName (..), ParamName (..), ParamValue (..), Query (..), QueryNode (..),
                       QueryParam (..), State (..))

import qualified Data.Text as T


renderTopQuery :: Query -> Text
renderTopQuery q = "query {\n" <> renderQuery 1 q <> "\n}\n"

renderQuery :: Int -> Query -> Text
renderQuery i Query{..} = T.intercalate "\n"
    $ map (renderQueryNode i) unQuery

renderQueryNode :: Int -> QueryNode -> Text
renderQueryNode i QueryNode{..} =
    tab i <> renderNodeName queryNodeName
    <> memptyIfTrue (null queryNodeArgs)
       (between "(" ")" (T.intercalate ", " $ map renderQueryParam queryNodeArgs))
    <> memptyIfTrue (null $ unQuery queryNode)
       (between " {\n" ("\n" <> tab i <> "}") (renderQuery (i + 1) queryNode))

renderNodeName :: NodeName -> Text
renderNodeName = \case
    NodeRepository   -> "repository"
    NodeIssues       -> "issues"
    NodePullRequests -> "pullRequests"
    NodeViewer       -> "viewer"
    NodeTitle        -> "title"
    NodeAuthor       -> "author"
    NodeLogin        -> "login"
    NodeResourcePath -> "resourcePath"
    NodeUrl          -> "url"
    NodeNodes        -> "nodes"
    NodeEdges        -> "edges"

renderQueryParam :: QueryParam -> Text
renderQueryParam QueryParam{..} =
    renderParamName queryParamName
    <> ": "
    <> renderParamValue queryParamValue

renderParamName :: ParamName -> Text
renderParamName = \case
    ParamOwner  -> "owner"
    ParamName   -> "name"
    ParamLast   -> "last"
    ParamStates -> "states"

renderParamValue :: ParamValue -> Text
renderParamValue = \case
    ParamStringV str -> T.pack $ show str
    ParamIntV i -> T.pack $ show i
    ParamStatesV (s :| ss) -> between "[" "]"
        $ T.intercalate ", " $ map renderState (s:ss)

renderState :: State -> Text
renderState = \case
    Open   -> "OPEN"
    Closed -> "CLOSED"
    Merged -> "MERGED"

between :: Text -> Text -> Text -> Text
between s e txt = s <> txt <> e

tab :: Int -> Text
tab i = stimes (i * 2) " "

memptyIfTrue :: Bool -> Text -> Text
memptyIfTrue p txt = if p then "" else txt
