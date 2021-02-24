{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

GraphQL AST renderer.
-}

module GitHub.Render
    ( -- * Query
      renderTopQuery

      -- * Mutation
    , renderTopMutation
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (stimes)
import Data.Text (Text)

import GitHub.GraphQL (IssueOrderField (..), Mutation (..), MutationFun (..), MutationNode (..),
                       NodeName (..), OrderDirection (..), ParamName (..), ParamValue (..),
                       Query (..), QueryNode (..), QueryParam (..), State (..))

import qualified Data.Text as T

----------------------------------------------------------------------------
-- Query
----------------------------------------------------------------------------

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
    NodeIssue        -> "issue"
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
    NodeCreateIssue  -> "createIssue"

renderQueryParam :: QueryParam -> Text
renderQueryParam QueryParam{..} =
    renderParamName queryParamName
    <> ": "
    <> renderParamValue queryParamValue

renderParamName :: ParamName -> Text
renderParamName = \case
    ParamOwner        -> "owner"
    ParamName         -> "name"
    ParamTitle        -> "title"
    ParamLast         -> "last"
    ParamStates       -> "states"
    ParamOrderBy      -> "orderBy"
    ParamField        -> "field"
    ParamDirection    -> "direction"
    ParamInput        -> "input"
    ParamRepositoryId -> "repositoryId"
    ParamMilestoneId  -> "milestoneId"

renderParamValue :: ParamValue -> Text
renderParamValue = \case
    ParamStringV str -> T.pack $ show str
    ParamIntV i -> T.pack $ show i
    ParamStatesV (s :| ss) -> between "[" "]"
        $ T.intercalate ", " $ map renderState (s:ss)
    ParamIssueOrderField io -> renderIssueOrderField io
    ParamOrderDirection d -> renderOrderDirection d
    ParamRecordV (p :| ps) -> between "{" "}"
        $ T.intercalate ", " $ map renderQueryParam (p:ps)

renderState :: State -> Text
renderState = \case
    Open   -> "OPEN"
    Closed -> "CLOSED"
    Merged -> "MERGED"

renderIssueOrderField :: IssueOrderField -> Text
renderIssueOrderField = \case
    Comments  -> "COMMENTS"
    CreatedAt -> "CREATED_AT"
    UpdatedAt -> "UPDATED_AT"

renderOrderDirection :: OrderDirection -> Text
renderOrderDirection = \case
    Asc  -> "ASC"
    Desc -> "DESC"

----------------------------------------------------------------------------
-- Mutation
----------------------------------------------------------------------------

renderTopMutation :: Mutation -> Text
renderTopMutation (Mutation funs) =
    "mutation {\n" <> T.intercalate "\n" (map renderMutationFun funs) <> "\n}\n"

renderMutationFun :: MutationFun -> Text
renderMutationFun MutationFun{..} =
    tab 1
    <> renderNodeName mutationFunName
    <> between "(" ")" (renderQueryParam inputParam)
    <> renderMutationNodes 2 mutationFunReturning
  where
    inputParam :: QueryParam
    inputParam = QueryParam
        { queryParamName  = ParamInput
        , queryParamValue = ParamRecordV mutationFunInput
        }

renderMutationNodes :: Int -> [MutationNode] -> Text
renderMutationNodes i nodes = memptyIfTrue
    (null nodes)
    ( between " {\n" ("\n" <> tab (i - 1) <> "}")
    $ T.intercalate "\n"
    $ map (\n -> tab i <> renderMutationNode i n) nodes
    )

renderMutationNode :: Int -> MutationNode -> Text
renderMutationNode i MutationNode{..} =
    renderNodeName mutationNodeName
    <> renderMutationNodes (i + 1) mutationNodeChildren

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

between :: Text -> Text -> Text -> Text
between s e txt = s <> txt <> e

tab :: Int -> Text
tab i = stimes (i * 2) " "

memptyIfTrue :: Bool -> Text -> Text
memptyIfTrue p txt = if p then "" else txt
