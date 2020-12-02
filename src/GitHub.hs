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

import GitHub.GraphQL (State (..))


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
    (RepositoryArgs { owner = "kowainik", name = "hit-on"})
    $ issues
        (IssuesArgs { last = 3, states = one Open })
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

{- | Arguments for the 'Repository' connection.
-}
-- TODO: change field names to more verbose and add lenses
data RepositoryArgs = RepositoryArgs
    { owner :: !Text
    , name  :: !Text
    }

{- | Fields and connections of the @Repository@ object:

* https://developer.github.com/v4/object/repository/
-}
data RepositoryField
    = RepositoryIssues Issues
    | RepositoryPullRequests PullRequests

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

{- | Arguments for the 'Issues' connection.
-}
data IssuesArgs = IssuesArgs
    { last   :: !Int
    , states :: !(NonEmpty State)
    }

{- | Fields of the @Issue@ object.

* https://developer.github.com/v4/object/issue/
-}
data IssueField
    = IssueTitle
    | IssueAuthor (NonEmpty AuthorField)

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

{- | Arguments for the 'PullRequest' connection.
-}
data PullRequestsArgs = PullRequestsArgs
    { pullRequestsLast   :: !Int
    , pullRequestsStates :: !(NonEmpty State)
    }

{- | Fields of the @PullRequest@ object.

* https://developer.github.com/v4/object/issue/
-}
data PullRequestField
    = PullRequestTitle
    | PullRequestAuthor (NonEmpty AuthorField)

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
