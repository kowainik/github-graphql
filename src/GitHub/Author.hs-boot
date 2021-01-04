module GitHub.Author
    ( AuthorField (..)
    , authorToAst
    ) where

import Data.List.NonEmpty (NonEmpty (..))

import GitHub.GraphQL (QueryNode)


data AuthorField
    = AuthorLogin
    | AuthorResourcePath
    | AuthorUrl

authorToAst :: NonEmpty AuthorField -> QueryNode