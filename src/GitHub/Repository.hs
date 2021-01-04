{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and helper functions to work with @repository@.
-}

module GitHub.Repository
    ( -- * Data types
      Repository (..)
    , RepositoryArgs (..)

      -- * Smart constructors
    , repository
    , issues
    , pullRequests

      -- * AST functions
    , repositoryToAst
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

import GitHub.Connection (Connection (..))
import GitHub.GraphQL (NodeName (..), ParamName (..), ParamValue (..), Query (..), QueryNode (..),
                       QueryParam (..), mkQuery)
import GitHub.Issues (IssueField, Issues (..), IssuesArgs, issuesToAst)
import GitHub.PullRequests (PullRequestField, PullRequests (..), PullRequestsArgs,
                            pullRequestsToAst)


{- | The @Repository@ top-level connection:

* https://developer.github.com/v4/query/#connections
-}
data Repository = Repository
    { repositoryArgs   :: !RepositoryArgs
    , repositoryFields :: !(NonEmpty RepositoryField)
    }

repositoryToAst :: Repository -> Query
repositoryToAst Repository{..} = Query
    [ QueryNode
        { queryNodeName = NodeRepository
        , queryNodeArgs = repositoryArgsToAst repositoryArgs
        , queryNode     = mkQuery repositoryFieldToAst repositoryFields
        }
    ]

{- | Arguments for the 'Repository' connection.
-}
data RepositoryArgs = RepositoryArgs
    { repositoryArgsOwner :: !Text
    , repositoryArgsName  :: !Text
    }

repositoryArgsToAst :: RepositoryArgs -> [QueryParam]
repositoryArgsToAst RepositoryArgs{..} =
    [ QueryParam
        { queryParamName = ParamOwner
        , queryParamValue = ParamStringV repositoryArgsOwner
        }
    , QueryParam
        { queryParamName = ParamName
        , queryParamValue = ParamStringV repositoryArgsName
        }
    ]

{- | Fields and connections of the @Repository@ object:

* https://developer.github.com/v4/object/repository/
-}
data RepositoryField
    = RepositoryIssues Issues
    | RepositoryPullRequests PullRequests

repositoryFieldToAst :: RepositoryField -> QueryNode
repositoryFieldToAst = \case
    RepositoryIssues issuesField             -> issuesToAst issuesField
    RepositoryPullRequests pullRequestsField -> pullRequestsToAst pullRequestsField

{- | Smart constructor for the 'Repository' type.
-}
repository :: RepositoryArgs -> NonEmpty RepositoryField -> Repository
repository repositoryArgs repositoryFields = Repository{..}

{- | Smart constructor for the 'Issue' field of the 'RepositoryField'.
-}
issues :: IssuesArgs -> NonEmpty (Connection IssueField) -> RepositoryField
issues issuesArgs issuesConnections = RepositoryIssues Issues{..}

{- | Smart constructor for the 'PullRequest' field of the 'RepositoryField'.
-}
pullRequests
    :: PullRequestsArgs
    -> NonEmpty (Connection PullRequestField)
    -> RepositoryField
pullRequests pullRequestsArgs pullRequestsConnections =
    RepositoryPullRequests PullRequests{..}
