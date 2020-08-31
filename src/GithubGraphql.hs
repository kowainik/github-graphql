{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

GraphQL bindings to GitHub API
-}

module GithubGraphql
    ( projectName
    ) where


projectName :: String
projectName = "github-graphql"

myQuery :: GraphQLQuery
myQuery = toGraphQL $
    repository
        & owner =. "kowainik"
        & name =. "stan"
        >. pullRequests
            & last =. 5
            & states =. [Closed, Open]
            >. nodes
               ?. title
               >. author
                   ?. login

newtype GraphQLQuery = GraphQLQuery Text

class ToGraphQL a where
    toGraphQL :: a -> GraphQLQuery

data Repository = Repository
    { rOwner :: !Text
    , rName  :: !Text
    , rQuery :: !(Map RepoNode Connection)
    }

instance ToGraphQL Repositry where
    toGraphQL Repository{..} = GrqphQLQuery $
      "repository(" <> "owner: " <> rOwner <> ", name: " <> rName <> ") {" <>
      <> toGraphQL rQuery
      <> "}"

data RepoNode
    = PullRequests
    | Issues

data Connection a = Connection
    { last     :: !Int
    , states   :: ![State]
    , nodes    :: ![a]
    , edges    :: ![Edge a]
    , pageInfo :: !()
    }

data Edge a = Edge
    { edgeCursor :: !Text
    , edgeNode   :: !a
    }

data State
    = Open
    | Closed
