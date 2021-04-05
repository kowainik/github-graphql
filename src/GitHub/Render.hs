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

import GitHub.GraphQL (IssueOrderField (..), IssueState (..), MilestoneOrderField (..),
                       Mutation (..), MutationFun (..), NodeName (..), OrderDirection (..),
                       ParamName (..), ParamValue (..), PullRequestState (..), Query (..),
                       QueryNode (..), QueryParam (..))

import qualified Data.Text as T

----------------------------------------------------------------------------
-- Query
----------------------------------------------------------------------------

renderTopQuery :: Query -> Text
renderTopQuery q = "query" <> renderQuery 1 q <> "\n"

renderQuery :: Int -> Query -> Text
renderQuery i Query{..} = between
    " {\n"
    ("\n" <> tab (i - 1) <> "}")
    (T.intercalate "\n" $ map (renderQueryNode i) unQuery)

renderQueryNode :: Int -> QueryNode -> Text
renderQueryNode i QueryNode{..} =
    tab i <> renderNodeName queryNodeName
    <> memptyIfTrue (null queryNodeArgs)
       (between "(" ")" (T.intercalate ", " $ map renderQueryParam queryNodeArgs))
    <> memptyIfTrue (null $ unQuery queryNode)
       (renderQuery (i + 1) queryNode)

renderNodeName :: NodeName -> Text
renderNodeName = \case
    NodeAddAssigneesToAssignable -> "addAssigneesToAssignable"
    NodeAssignees                -> "assignees"
    NodeAuthor                   -> "author"
    NodeBody                     -> "body"
    NodeClientMutationId         -> "clientMutationId"
    NodeCreateIssue              -> "createIssue"
    NodeEdges                    -> "edges"
    NodeId                       -> "id"
    NodeIssue                    -> "issue"
    NodeIssues                   -> "issues"
    NodeLabels                   -> "labels"
    NodeLogin                    -> "login"
    NodeMilestone                -> "milestone"
    NodeMilestones               -> "milestones"
    NodeName                     -> "name"
    NodeNodes                    -> "nodes"
    NodeNumber                   -> "number"
    NodeProgressPercentage       -> "progressPercentage"
    NodePullRequests             -> "pullRequests"
    NodeRepository               -> "repository"
    NodeResourcePath             -> "resourcePath"
    NodeState                    -> "state"
    NodeTitle                    -> "title"
    NodeTotalCount               -> "totalCount"
    NodeUrl                      -> "url"
    NodeViewer                   -> "viewer"

renderQueryParam :: QueryParam -> Text
renderQueryParam QueryParam{..} =
    renderParamName queryParamName
    <> ": "
    <> renderParamValue queryParamValue

renderParamName :: ParamName -> Text
renderParamName = \case
    ParamAssignableId -> "assignableId"
    ParamAssigneeIds  -> "assigneeIds"
    ParamDirection    -> "direction"
    ParamField        -> "field"
    ParamHeadRefName  -> "headRefName"
    ParamInput        -> "input"
    ParamLast         -> "last"
    ParamMilestoneId  -> "milestoneId"
    ParamName         -> "name"
    ParamNumber       -> "number"
    ParamOrderBy      -> "orderBy"
    ParamOwner        -> "owner"
    ParamRepositoryId -> "repositoryId"
    ParamStates       -> "states"
    ParamTitle        -> "title"

renderParamValue :: ParamValue -> Text
renderParamValue = \case
    ParamStringV str -> T.pack $ show str
    ParamIntV i -> T.pack $ show i
    ParamIssueStatesV (s :| ss) -> renderList renderIssueState (s : ss)
    ParamPullRequestStatesV (s :| ss) -> renderList renderPullRequestState (s : ss)
    ParamIssueOrderField io -> renderIssueOrderField io
    ParamMilestoneOrderField io -> renderMilestoneOrderField io
    ParamOrderDirection d -> renderOrderDirection d
    ParamRecordV (p :| ps) -> between "{" "}"
        $ T.intercalate ", " $ map renderQueryParam (p:ps)
    ParamArrayV array -> renderList renderParamValue array
  where
    renderList :: (a -> Text) -> [a] -> Text
    renderList render = between "[" "]" . T.intercalate ", " . map render

renderIssueState :: IssueState -> Text
renderIssueState = \case
    IssueOpen   -> "OPEN"
    IssueClosed -> "CLOSED"

renderPullRequestState :: PullRequestState -> Text
renderPullRequestState = \case
    PullRequestOpen   -> "OPEN"
    PullRequestClosed -> "CLOSED"
    PullRequestMerged -> "MERGED"

renderIssueOrderField :: IssueOrderField -> Text
renderIssueOrderField = \case
    Comments  -> "COMMENTS"
    CreatedAt -> "CREATED_AT"
    UpdatedAt -> "UPDATED_AT"

renderMilestoneOrderField :: MilestoneOrderField -> Text
renderMilestoneOrderField = \case
    MCreatedAt -> "CREATED_AT"
    MDueDate   -> "DUE_DATE"
    MUpdatedAt -> "UPDATED_AT"
    MNumber    -> "NUMBER"

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
    <> renderQuery 2 (Query mutationFunReturning)
  where
    inputParam :: QueryParam
    inputParam = QueryParam
        { queryParamName  = ParamInput
        , queryParamValue = ParamRecordV mutationFunInput
        }

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

between :: Text -> Text -> Text -> Text
between s e txt = s <> txt <> e

tab :: Int -> Text
tab 0 = ""
tab i = stimes (i * 2) " "

memptyIfTrue :: Bool -> Text -> Text
memptyIfTrue p txt = if p then "" else txt
