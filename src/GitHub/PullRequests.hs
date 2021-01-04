{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and helper functions to work with @pullRequests@.
-}

module GitHub.PullRequests
    ( -- * Data types
      PullRequests (..)
    , PullRequestsArgs (..)
    , PullRequestField (..)

      -- * AST functions
    , pullRequestsToAst
    ) where

import Data.List.NonEmpty (NonEmpty (..))

import {-# SOURCE #-} GitHub.Author (AuthorField, authorToAst)
import GitHub.Connection (Connection (..), connectionToAst)
import GitHub.GraphQL (NodeName (..), ParamName (..), ParamValue (..), QueryNode (..),
                       QueryParam (..), State (..), mkQuery, nameNode)


{- | The @pullRequests@ connection of the 'Repository' object.

* https://developer.github.com/v4/object/repository/#connections
-}
data PullRequests = PullRequests
    { pullRequestsArgs        :: !PullRequestsArgs
    , pullRequestsConnections :: !(NonEmpty (Connection PullRequestField))
    }

pullRequestsToAst :: PullRequests -> QueryNode
pullRequestsToAst PullRequests{..} = QueryNode
    { queryNodeName = NodePullRequests
    , queryNodeArgs = pullRequestsArgsToAst pullRequestsArgs
    , queryNode     = mkQuery (connectionToAst pullRequestFieldToAst) pullRequestsConnections
    }

{- | Arguments for the 'PullRequest' connection.
-}
data PullRequestsArgs = PullRequestsArgs
    { pullRequestsLast   :: !Int
    , pullRequestsStates :: !(NonEmpty State)
    }

pullRequestsArgsToAst :: PullRequestsArgs -> [QueryParam]
pullRequestsArgsToAst PullRequestsArgs{..} =
    [ QueryParam
        { queryParamName = ParamLast
        , queryParamValue = ParamIntV pullRequestsLast
        }
    , QueryParam
        { queryParamName = ParamStates
        , queryParamValue = ParamStatesV pullRequestsStates
        }
    ]

{- | Fields of the @PullRequest@ object.

* https://developer.github.com/v4/object/issue/
-}
data PullRequestField
    = PullRequestTitle
    | PullRequestAuthor (NonEmpty AuthorField)

pullRequestFieldToAst :: PullRequestField -> QueryNode
pullRequestFieldToAst = \case
    PullRequestTitle               -> nameNode NodeTitle
    PullRequestAuthor authorFields -> authorToAst authorFields
