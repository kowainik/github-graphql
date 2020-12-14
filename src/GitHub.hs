{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

GraphQL bindings to GitHub API.
-}

module GitHub
    ( -- * Top-level queries
      Repository (..)
    , RepositoryArgs (..)
    , repository
    , repositoryToAst

      -- * Queries connections
      -- ** Issues
    , Issues (..)
    , IssuesArgs (..)
    , issues
      -- ** PullRequests
    , PullRequests (..)
    , PullRequestsArgs (..)
    , pullRequests

      -- * Connection
    , Connection (..)
    , nodes

      -- * Connections fields
      -- ** Interfaces
    , HasAuthor (..)
    , HasTitle (..)
      -- ** Data
    , AuthorField (..)
    , IssueField (..)
    , PullRequestField (..)

      -- * Temp
    , exampleQuery
    , projectName
    ) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

import GitHub.GraphQL (NodeName (..), ParamName (..), ParamValue (..), Query (..), QueryNode (..),
                       QueryParam (..), State (..), mkQuery, nameNode)


projectName :: String
projectName = "github-graphql"

-- TODO: temporary helper function
one :: a -> NonEmpty a
one x = x :| []

{- Example of the following GraphQL query:

@
query {
  repository(owner: "kowainik", name: "hit-on") {
    issues(last: 3, states: [OPEN]) {
        nodes {
          title
          author{
            login
          }
        }
    }

    pullRequests(last: 3, states: [OPEN]) {
        nodes {
          title
          author {
            login
          }
        }
    }
  }
}
@
-}
exampleQuery :: Repository
exampleQuery = repository
    (RepositoryArgs { repositoryArgsOwner = "kowainik", repositoryArgsName = "hit-on"})
    $ issues
        (IssuesArgs { issuesArgsLast = 3, issuesArgsStates = one Open })
        (one $ nodes $
           title :|
           [author $ one login]
        )
    :|
    [ pullRequests
        (PullRequestsArgs { pullRequestsLast = 3, pullRequestsStates = one Open })
        (one $ nodes $
           title :|
           [author $ one login]
        )

    ]

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


{- | The @issues@ connection of the 'Repository' object.

* https://developer.github.com/v4/object/repository/#connections
-}
data Issues = Issues
    { issuesArgs        :: !IssuesArgs
    , issuesConnections :: !(NonEmpty (Connection Issues))
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

{- | Smart constructor for the 'Issue' field of the 'RepositoryField'.
-}
issues :: IssuesArgs -> NonEmpty (Connection Issues) -> RepositoryField
issues issuesArgs issuesConnections = RepositoryIssues Issues{..}

{- | The @pullRequests@ connection of the 'Repository' object.

* https://developer.github.com/v4/object/repository/#connections
-}
data PullRequests = PullRequests
    { pullRequestsArgs        :: !PullRequestsArgs
    , pullRequestsConnections :: !(NonEmpty (Connection PullRequests))
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

{- | Smart constructor for the 'PullRequest' field of the 'RepositoryField'.
-}
pullRequests
    :: PullRequestsArgs
    -> NonEmpty (Connection PullRequests)
    -> RepositoryField
pullRequests pullRequestsArgs pullRequestsConnections =
    RepositoryPullRequests PullRequests{..}

{- | Generic type for connections since they share the same
fields. Examples are:

* IssueConnection: https://developer.github.com/v4/object/issueconnection/
* PullRequestConnection: https://developer.github.com/v4/object/pullrequestconnection/
-}
data Connection obj
    = Nodes !(NonEmpty (ObjectFields obj))
    | Edges

connectionToAst :: (ObjectFields obj -> QueryNode) -> Connection obj -> QueryNode
connectionToAst objToAst = \case
    Nodes fields -> QueryNode
        { queryNodeName = NodeNodes
        , queryNodeArgs = []
        , queryNode = mkQuery objToAst fields
        }
    Edges -> nameNode NodeEdges

{- | Type-level function to map objects to their corresponding field types.
-}
type family ObjectFields (obj :: Type) :: Type where
    ObjectFields Issues       = IssueField
    ObjectFields PullRequests = PullRequestField
--    ObjectFields _ = ... custom type error ...

{- | Smart constructor for the 'Nodes' 'Connection'.
-}
nodes :: NonEmpty (ObjectFields obj) -> Connection obj
nodes = Nodes


{- | Typeclass for objects that have a @title@ field.
-}
class HasTitle field where
    title :: field

instance HasTitle IssueField where
    title :: IssueField
    title = IssueTitle

instance HasTitle PullRequestField where
    title :: PullRequestField
    title = PullRequestTitle

{- | Fields of the @Actor@ object.

* https://developer.github.com/v4/interface/actor/
-}
-- TODO: rename data type to 'ActorField'?
data AuthorField
    = AuthorLogin
    | AuthorResourcePath
    | AuthorUrl

authorToAst :: NonEmpty AuthorField -> QueryNode
authorToAst authorFields = QueryNode
    { queryNodeName = NodeAuthor
    , queryNodeArgs = []
    , queryNode     = mkQuery authorFieldToAst authorFields
    }

authorFieldToAst :: AuthorField -> QueryNode
authorFieldToAst = \case
    AuthorLogin        -> nameNode NodeLogin
    AuthorResourcePath -> nameNode NodeResourcePath
    AuthorUrl          -> nameNode NodeUrl

login :: AuthorField
login = AuthorLogin

{- | Typeclass for objects that have an @author@ field.
-}
class HasAuthor field where
    author :: NonEmpty AuthorField -> field

instance HasAuthor IssueField where
    author :: NonEmpty AuthorField -> IssueField
    author = IssueAuthor

instance HasAuthor PullRequestField where
    author :: NonEmpty AuthorField -> PullRequestField
    author = PullRequestAuthor
