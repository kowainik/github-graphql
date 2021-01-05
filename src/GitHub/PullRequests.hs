{-# LANGUAGE DataKinds #-}

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
    , defPullRequestsArgs

    , PullRequestField (..)

      -- * AST functions
    , pullRequestsToAst
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Prolens (lens)

import {-# SOURCE #-} GitHub.Author (AuthorField, authorToAst)
import GitHub.Connection (Connection (..), connectionToAst)
import GitHub.GraphQL (NodeName (..), ParamName (..), ParamValue (..), QueryNode (..),
                       QueryParam (..), State (..), mkQuery, nameNode)
import GitHub.Lens (ArgsType (..), HasLimit (..), HasStates (..))


{- | The @pullRequests@ connection of the 'Repository' object.

* https://developer.github.com/v4/object/repository/#connections
-}
data PullRequests = PullRequests
    { pullRequestsArgs        :: !(PullRequestsArgs '[])
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
data PullRequestsArgs (args :: [ArgsType]) = PullRequestsArgs
    { pullRequestsArgsLast   :: !Int
    , pullRequestsArgsStates :: !(NonEmpty State)
    }

instance HasLimit PullRequestsArgs where
    lastL = lens pullRequestsArgsLast (\args new -> args { pullRequestsArgsLast = new })
    {-# INLINE lastL #-}

instance HasStates PullRequestsArgs where
    statesL = lens pullRequestsArgsStates (\args new -> args { pullRequestsArgsStates = new })
    {-# INLINE statesL #-}

{- | Default value of 'PullRequestsArgs'. Use methods of 'HasLimit' and
'HasStates' to change its fields.
-}
defPullRequestsArgs :: PullRequestsArgs '[ 'ArgsLimit, 'ArgsStates ]
defPullRequestsArgs = PullRequestsArgs
    { pullRequestsArgsLast = -1
    , pullRequestsArgsStates = Open :| []
    }

pullRequestsArgsToAst :: PullRequestsArgs '[] -> [QueryParam]
pullRequestsArgsToAst PullRequestsArgs{..} =
    [ QueryParam
        { queryParamName = ParamLast
        , queryParamValue = ParamIntV pullRequestsArgsLast
        }
    , QueryParam
        { queryParamName = ParamStates
        , queryParamValue = ParamStatesV pullRequestsArgsStates
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
