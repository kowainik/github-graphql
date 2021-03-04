{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

GraphQL AST.
-}

module GitHub.GraphQL
    ( -- * Query
      -- ** Top-level AST types
      Query (..)
    , mkQuery
      -- ** Nested AST types
    , QueryNode (..)
    , nameNode

    , NodeName (..)
    , QueryParam (..)
    , ParamName (..)
    , ParamValue (..)

      -- * Mutation
    , Mutation (..)
    , MutationFun (..)

      -- * Enums
      -- ** States
    , State (..)
    , merged
    , IssueState (..)
    , PullRequestState (..)
      -- ** Other
    , IssueOrderField (..)
    , OrderDirection (..)

      -- * Utils
    , one
    ) where

import Data.Aeson (FromJSON (..), withText)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

import qualified Data.Text as Text

----------------------------------------------------------------------------
-- Queries
----------------------------------------------------------------------------

newtype Query = Query
    { unQuery :: [QueryNode]
    } deriving stock (Show)

{- | Smart constructor for creating 'Query' from any 'Foldable'
container (e.g. list of fields, 'NonEmpty' of smth, etc.)
-}
mkQuery :: Foldable f => (n -> QueryNode) -> f n -> Query
mkQuery toNode = Query . map toNode . toList

{- | Recursive data type with name, parameters and children of the
same type.
-}
data QueryNode = QueryNode
    { queryNodeName :: NodeName
    , queryNodeArgs :: [QueryParam]
    , queryNode     :: Query
    } deriving stock (Show)

{- | Create 'QueryNode' with node fields and no subquery. E.g. @title@
or @name@ in the example below.

@
nodes {
    title
    names
}
@
-}
nameNode :: NodeName -> QueryNode
nameNode name = QueryNode
    { queryNodeName = name
    , queryNodeArgs = []
    , queryNode     = Query []
    }

data NodeName
    = NodeAssignees
    | NodeAuthor
    | NodeBody
    | NodeCreateIssue
    | NodeEdges
    | NodeId
    | NodeIssue
    | NodeIssues
    | NodeLabels
    | NodeLogin
    | NodeName
    | NodeNodes
    | NodeNumber
    | NodePullRequests
    | NodeRepository
    | NodeResourcePath
    | NodeState
    | NodeTitle
    | NodeUrl
    | NodeViewer
    deriving stock (Show)

data QueryParam = QueryParam
    { queryParamName  :: ParamName
    , queryParamValue :: ParamValue
    } deriving stock (Show)

data ParamName
    = ParamOwner
    | ParamName
    | ParamTitle
    | ParamLast
    | ParamStates
    | ParamOrderBy
    | ParamField
    | ParamDirection
    | ParamInput
    | ParamRepositoryId
    | ParamMilestoneId
    deriving stock (Show)

data ParamValue
    {- | Textual parameter:

    @
    name: "github-graphql"
    @
    -}
    = ParamStringV Text

    {- | Integer parameter:

    @
    last: 2
    @
    -}
    | ParamIntV Int

    {- | Issue states:

    @
    states: [OPEN, CLOSED]"
    @
    -}
    | ParamIssueStatesV (NonEmpty IssueState)

    {- | PR states:

    @
    states: [OPEN, CLOSED, MERGED]"
    @
    -}
    | ParamPullRequestStatesV (NonEmpty PullRequestState)

    {- | Issues order field:

    @
    field: CREATED_AT
    @
    -}
    | ParamIssueOrderField IssueOrderField

    {- | Direction of order:

    @
    direction: ASC
    @
    -}
    | ParamOrderDirection OrderDirection

    {- | Record parameters:

    @
    orderBy: {field: CREATED_AT, direction: DESC}
    @
    -}
    | ParamRecordV (NonEmpty QueryParam)
    deriving stock (Show)

----------------------------------------------------------------------------
-- Mutation
----------------------------------------------------------------------------

newtype Mutation = Mutation
    { unMutation :: [MutationFun]
    } deriving stock (Show)

data MutationFun = MutationFun
    { mutationFunName      :: NodeName
    , mutationFunInput     :: NonEmpty QueryParam
    , mutationFunReturning :: [QueryNode]
    } deriving stock (Show)

----------------------------------------------------------------------------
-- Enums
----------------------------------------------------------------------------

data IssueState
    = IssueOpen
    | IssueClosed
    deriving stock (Show, Eq)

instance FromJSON IssueState where
    parseJSON = withText "IssueState" $ \case
        "OPEN"   -> pure IssueOpen
        "CLOSED" -> pure IssueClosed
        other    -> fail $ "Expected OPEN or CLOSED but got: " <> Text.unpack other

data PullRequestState
    = PullRequestOpen
    | PullRequestClosed
    | PullRequestMerged
    deriving stock (Show, Eq)

instance FromJSON PullRequestState where
    parseJSON = withText "PullRequestState" $ \case
        "OPEN"   -> pure PullRequestOpen
        "CLOSED" -> pure PullRequestClosed
        "MERGED" -> pure PullRequestMerged
        other    -> fail $ "Expected OPEN or CLOSED or MERGED but got: " <> Text.unpack other

merged :: PullRequestState
merged = PullRequestMerged

data IssueOrderField
    = Comments
    | CreatedAt
    | UpdatedAt
    deriving stock (Show)

data OrderDirection
    = Asc
    | Desc
    deriving stock (Show)

----------------------------------------------------------------------------
-- Interfaces
----------------------------------------------------------------------------

class State s where
    open   :: s
    closed :: s

instance State IssueState where
    open   = IssueOpen
    closed = IssueClosed

instance State PullRequestState where
    open   = PullRequestOpen
    closed = PullRequestClosed

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

{- | Helper function to construct singleton 'NonEmpty' lists.
-}
one :: a -> NonEmpty a
one x = x :| []
