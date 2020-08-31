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

myQuery :: Repository
myQuery =
    repository
        & set owner "kowainik"
        & set name  "stan"
        >. (pullRequests
            & set last 5
            & set states [Closed, Open]
            >. (nodes
               ?. title
               >. (author
                   ?. login
                  )
               )
           )

--        >. (issues ...
--           )

-- newtype GraphQLQuery = GraphQLQuery Text
--
-- class ToGraphQL a where
--     toGraphQL :: a -> GraphQLQuery

infixl 0 >.
(>.) :: Repository -> RepositoryPayload -> Repository
repo >. payload = repo { rQuery = payload : rQuery repo }


data Repository = Repository
    { rOwner :: !Text
    , rName  :: !Text
    , rQuery :: ![RepositoryPayload]
    }

repository :: Repository
repository = Repository
    { rOwner = ""
    , rName = ""
    , rQuery = []
    }

-- instance ToGraphQL Repositry where
--     toGraphQL Repository{..} = GrqphQLQuery $
--       "repository(" <> "owner: " <> rOwner <> ", name: " <> rName <> ") {" <>
--       <> toGraphQL rQuery
--       <> "}"

data RepositoryConnection
    = RCPullRequests
    | RCIssues

data SRepositoryConnection (t :: RepositoryConnection) where
    SRCPullRequests :: SRepositoryConnection 'RCPullRequests
    SRCIssues       :: SRepositoryConnection 'RCIssues

data RepositoryPayload where
    RepositoryPayload :: SRepositoryConnection t -> Connection (ToRepositoryPayload t) -> RepositoryPayload

type family ToRepositoryPayload (t :: RepositoryConnection) :: Type where
    ToRepositoryPayload 'RCPullRequests = PullRequest

data Connection a = Connection
    { last     :: !Int
    , states   :: ![State]
    , nodes    :: !a
--    , edges    :: !(Edge a)
--    , pageInfo :: !()
    }

pullRequests :: Connection PullRequest
pullRequests = Connection
    { last = 0
    , states = []
    , nodes = PullRequest
        { prTitle = ""
        , prAuthor = Actor
            { actorLogin = ""
            }
        }
    ,
    }

data Edge a = Edge
    { edgeCursor :: !Text
    , edgeNode   :: !a
    }

data State
    = Open
    | Closed

data PullRequest = PullRequest
    { prTitle :: !Text
    , prAuthor :: !Actor
    }

newtype Actor = Actor
    { actorLogin :: !Text
    }
