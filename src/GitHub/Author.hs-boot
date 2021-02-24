module GitHub.Author
    ( AuthorField (..)
    , authorToAst
    , authorToMutationNode
    ) where

import Data.List.NonEmpty (NonEmpty (..))

import GitHub.GraphQL (MutationNode, QueryNode)


data AuthorField
    = AuthorLogin
    | AuthorResourcePath
    | AuthorUrl

authorToAst :: NonEmpty AuthorField -> QueryNode
authorToMutationNode :: NonEmpty AuthorField -> MutationNode