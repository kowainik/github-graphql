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
    , MilestoneOrderField (..)
    , OrderDirection (..)
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
      deriving newtype (Semigroup, Monoid)

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
    = NodeAddAssigneesToAssignable
    | NodeAssignees
    | NodeAuthor
    | NodeBody
    | NodeClientMutationId
    | NodeCreateIssue
    | NodeDescription
    | NodeEdges
    | NodeId
    | NodeIssue
    | NodeIssues
    | NodeLabels
    | NodeLogin
    | NodeMilestone
    | NodeMilestones
    | NodeName
    | NodeNodes
    | NodeNumber
    | NodeProgressPercentage
    | NodePullRequests
    | NodeRepository
    | NodeResourcePath
    | NodeState
    | NodeTitle
    | NodeTotalCount
    | NodeUrl
    | NodeViewer
    deriving stock (Show)

data QueryParam = QueryParam
    { queryParamName  :: ParamName
    , queryParamValue :: ParamValue
    } deriving stock (Show)

data ParamName
    = ParamAssignableId
    | ParamAssigneeIds
    | ParamDirection
    | ParamField
    | ParamHeadRefName
    | ParamInput
    | ParamLast
    | ParamMilestoneId
    | ParamName
    | ParamNumber
    | ParamOrderBy
    | ParamOwner
    | ParamRepositoryId
    | ParamStates
    | ParamTitle
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

    {- | Milestones order field:

    @
    field: NUMBER
    @
    -}
    | ParamMilestoneOrderField MilestoneOrderField

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

    {- | List parameters:

    @
    assignees: ["foo", "bar"]
    @
    -}
    -- TODO: use GADT to allow only single type
    | ParamArrayV [ParamValue]
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
    deriving stock (Show, Eq, Enum, Bounded)

instance FromJSON IssueState where
    parseJSON = withText "IssueState" $ \case
        "OPEN"   -> pure IssueOpen
        "CLOSED" -> pure IssueClosed
        other    -> fail $ "Expected OPEN or CLOSED but got: " <> Text.unpack other

data PullRequestState
    = PullRequestOpen
    | PullRequestClosed
    | PullRequestMerged
    deriving stock (Show, Eq, Enum, Bounded)

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
    deriving stock (Show, Eq, Enum, Bounded)

-- https://docs.github.com/en/graphql/reference/enums#milestoneorderfield
data MilestoneOrderField
    = MCreatedAt
    | MDueDate
    | MUpdatedAt
    | MNumber
    deriving stock (Show, Eq, Enum, Bounded)

data OrderDirection
    = Asc
    | Desc
    deriving stock (Show, Eq, Enum, Bounded)

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
