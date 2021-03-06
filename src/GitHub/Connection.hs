{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and helper functions to work with connections and nodes.
-}

module GitHub.Connection
    ( -- * Data types
      Connection (..)
      -- * Smart constructors
    , nodes
      -- * AST functions
    , connectionToAst
    ) where

import Data.List.NonEmpty (NonEmpty (..))

import GitHub.GraphQL (NodeName (..), QueryNode (..), mkQuery, nameNode)


{- | Generic type for connections since they share the same
fields. Examples are:

* IssueConnection: https://developer.github.com/v4/object/issueconnection/
* PullRequestConnection: https://developer.github.com/v4/object/pullrequestconnection/
-}
data Connection fld
    = Nodes !(NonEmpty fld)
    | Edges
    | TotalCount

connectionToAst :: (fld -> QueryNode) -> Connection fld -> QueryNode
connectionToAst fldToAst = \case
    Nodes fields -> QueryNode
        { queryNodeName = NodeNodes
        , queryNodeArgs = []
        , queryNode = mkQuery fldToAst fields
        }
    Edges -> nameNode NodeEdges
    TotalCount -> nameNode NodeTotalCount

{- | Smart constructor for the 'Nodes' 'Connection'.
-}
nodes :: NonEmpty fld -> Connection fld
nodes = Nodes
