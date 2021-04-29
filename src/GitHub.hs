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
    , module GitHub.Issue
    , module GitHub.Order

      -- ** PullRequests
    , module GitHub.PullRequests

      -- ** Milestones
    , module GitHub.Milestone

      -- ** Issue or PR labels
    , module GitHub.Label

      -- ** GitHub users
    , module GitHub.User

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

      -- * General utils
    , module GitHub.Json
    , one
    ) where

import GitHub.Author
import GitHub.Connection
import GitHub.GraphQL
import GitHub.Id
import GitHub.Issue
import GitHub.Json
import GitHub.Label
import GitHub.Lens
import GitHub.Milestone
import GitHub.Order
import GitHub.PullRequests
import GitHub.Query
import GitHub.Repository
import GitHub.Title
import GitHub.User

import GitHub.Common (one)
