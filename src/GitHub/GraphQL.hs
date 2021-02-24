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
    , MutationNode (..)
    , nameMutationNode

      -- * Enums
    , IssueOrderField (..)
    , OrderDirection (..)
    , State (..)

      -- * Utils
    , one
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

import qualified Data.List.NonEmpty as NE

----------------------------------------------------------------------------
-- Queries
----------------------------------------------------------------------------

newtype Query = Query
    { unQuery :: [QueryNode]
    } deriving stock (Show)

{- | Smart constructor for creating 'Query' from 'NonEmpty' list (the
most common case).
-}
mkQuery :: (n -> QueryNode) -> NonEmpty n -> Query
mkQuery toNode nodes =
    Query $ NE.toList $ toNode <$> nodes

data QueryNode = QueryNode
    { queryNodeName :: !NodeName
    , queryNodeArgs :: ![QueryParam]
    , queryNode     :: !Query
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
    = NodeRepository
    | NodeIssue
    | NodeIssues
    | NodePullRequests
    | NodeViewer
    | NodeTitle
    | NodeAuthor
    | NodeLogin
    | NodeResourcePath
    | NodeUrl
    | NodeNodes
    | NodeEdges
    | NodeCreateIssue
    | NodeId
    deriving stock (Show)

data QueryParam = QueryParam
    { queryParamName  :: !ParamName
    , queryParamValue :: !ParamValue
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
    = ParamStringV !Text

    {- | Integer parameter:

    @
    last: 2
    @
    -}
    | ParamIntV !Int

    {- | Issue/PR states:

    @
    states: [CLOSED, MERGED]"
    @
    -}
    | ParamStatesV !(NonEmpty State)

    {- | Issues order field:

    @
    field: CREATED_AT
    @
    -}
    | ParamIssueOrderField !IssueOrderField

    {- | Direction of order:

    @
    direction: ASC
    @
    -}
    | ParamOrderDirection !OrderDirection

    {- | Record parameters:

    @
    orderBy: {field: CREATED_AT, direction: DESC}
    @
    -}
    | ParamRecordV !(NonEmpty QueryParam)
    deriving stock (Show)

----------------------------------------------------------------------------
-- Mutation
----------------------------------------------------------------------------

newtype Mutation = Mutation
    { unMutation :: [MutationFun]
    } deriving stock (Show)

data MutationFun = MutationFun
    { mutationFunName      :: !NodeName
    , mutationFunInput     :: !(NonEmpty QueryParam)
    , mutationFunReturning :: ![MutationNode]
    } deriving stock (Show)

-- TODO: maybe think how to unify with QueryNode?
data MutationNode = MutationNode
    { mutationNodeName     :: !NodeName
    , mutationNodeChildren :: ![MutationNode]
    } deriving stock (Show)

{- | Similar to 'nameNode' but for 'MutationNode'.
-}
nameMutationNode :: NodeName -> MutationNode
nameMutationNode name = MutationNode
    { mutationNodeName = name
    , mutationNodeChildren = []
    }

----------------------------------------------------------------------------
-- Enums
----------------------------------------------------------------------------

-- TODO: rename to PullRequestState
data State
    = Open
    | Closed
    | Merged
    deriving stock (Show)

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
-- Utils
----------------------------------------------------------------------------

{- | Helper function to construct singleton 'NonEmpty' lists.
-}
one :: a -> NonEmpty a
one x = x :| []
