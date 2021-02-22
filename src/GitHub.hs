{- |
Copyright: (c) 2020-2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

GraphQL bindings to GitHub API.
-}

module GitHub
    ( -- * Querying GitHub
      module GitHub.Query

      -- * Top-level queries
    , module GitHub.Repository

      -- * Queries connections
      -- ** Issues
    , module GitHub.Issues

      -- ** PullRequests
    , module GitHub.PullRequests

      -- * Connection
    , module GitHub.Connection

      -- * Connections fields
      -- ** Interfaces
    , module GitHub.Author
    , module GitHub.Title

      -- * Low-level AST fields
    , module GitHub.GraphQL

      -- * General tools to work with API
      -- ** Using lenses to change fields
    , module GitHub.Lens

      -- * General types
    , module GitHub.Id

      -- * Temp
    , exampleQuery
    , projectName
    , one
    ) where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Prolens (set)


import GitHub.Author
import GitHub.Connection
import GitHub.GraphQL (IssueOrderField (..), OrderDirection (..), State (..))
import GitHub.Id
import GitHub.Issues
import GitHub.Lens
import GitHub.PullRequests
import GitHub.Query
import GitHub.Repository
import GitHub.Title


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
    ( defRepositoryArgs
    & set ownerL "kowainik"
    & set nameL  "hit-on"
    )
    $ issues
        ( defIssuesArgs
        & set lastL 3
        & set statesL (one Open)
        )
        (one $ nodes $
           title :|
           [author $ one login]
        )
    :|
    [ pullRequests
        ( defPullRequestsArgs
        & set lastL 3
        & set statesL (one Open)
        )
        (one $ nodes $
           title :|
           [author $ one login]
        )
    ]
