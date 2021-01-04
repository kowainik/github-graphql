{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and helper functions to work with @issues@.
-}

module GitHub.Issues
    ( -- * Data types
      Issues (..)
    , IssuesArgs (..)
    , IssueField (..)

      -- * AST functions
    , issuesToAst
    ) where

import Data.List.NonEmpty (NonEmpty (..))

import {-# SOURCE #-} GitHub.Author (AuthorField, authorToAst)
import GitHub.Connection (Connection (..), connectionToAst)
import GitHub.GraphQL (NodeName (..), ParamName (..), ParamValue (..), QueryNode (..),
                       QueryParam (..), State (..), mkQuery, nameNode)


{- | The @issues@ connection of the 'Repository' object.

* https://developer.github.com/v4/object/repository/#connections
-}
data Issues = Issues
    { issuesArgs        :: !IssuesArgs
    , issuesConnections :: !(NonEmpty (Connection IssueField))
    }

issuesToAst :: Issues -> QueryNode
issuesToAst Issues{..} = QueryNode
    { queryNodeName = NodeIssues
    , queryNodeArgs = issuesArgsToAst issuesArgs
    , queryNode     = mkQuery (connectionToAst issueFieldToAst) issuesConnections
    }

{- | Arguments for the 'Issues' connection.
-}
data IssuesArgs = IssuesArgs
    { issuesArgsLast   :: !Int
    , issuesArgsStates :: !(NonEmpty State)
    }

issuesArgsToAst :: IssuesArgs -> [QueryParam]
issuesArgsToAst IssuesArgs{..} =
    [ QueryParam
        { queryParamName = ParamLast
        , queryParamValue = ParamIntV issuesArgsLast
        }
    , QueryParam
        { queryParamName = ParamStates
        , queryParamValue = ParamStatesV issuesArgsStates
        }
    ]

{- | Fields of the @Issue@ object.

* https://developer.github.com/v4/object/issue/
-}
data IssueField
    = IssueTitle
    | IssueAuthor (NonEmpty AuthorField)

issueFieldToAst :: IssueField -> QueryNode
issueFieldToAst = \case
    IssueTitle               -> nameNode NodeTitle
    IssueAuthor authorFields -> authorToAst authorFields
