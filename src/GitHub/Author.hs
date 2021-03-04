{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and helper functions to work with @author@/@actor@.
-}

module GitHub.Author
    ( -- * Data types
      AuthorField (..)
    , HasAuthor (..)

      -- * Smart constructors
    , login

      -- * AST functions
    , authorToAst
    ) where

import Data.List.NonEmpty (NonEmpty (..))

import GitHub.GraphQL (NodeName (..), QueryNode (..), mkQuery, nameNode)
import GitHub.Issues (IssueField (..))
import GitHub.PullRequests (PullRequestField (..))


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
