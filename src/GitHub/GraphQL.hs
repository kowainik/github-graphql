{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

GraphQL AST.
-}

module GitHub.GraphQL
    ( Query (..)
    , mkQuery

    , QueryNode (..)
    , nameNode

    , NodeName (..)
    , QueryParam (..)
    , ParamName (..)
    , ParamValue (..)
    , State (..)
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

import qualified Data.List.NonEmpty as NE


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
    deriving stock (Show)

data QueryParam = QueryParam
    { queryParamName  :: !ParamName
    , queryParamValue :: !ParamValue
    } deriving stock (Show)

data ParamName
    = ParamOwner
    | ParamName
    | ParamLast
    | ParamStates
    deriving stock (Show)

data ParamValue
    = ParamStringV !Text
    | ParamIntV !Int
    | ParamStatesV !(NonEmpty State)
    deriving stock (Show)

data State
    = Open
    | Closed
    | Merged
    deriving stock (Show)
