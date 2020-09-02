{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

GraphQL bindings to GitHub API
-}

module GithubGraphql
    ( projectName
    ) where

import Data.Kind (Type)
import Data.Text (Text)


projectName :: String
projectName = "github-graphql"

-- myQuery :: Repository
-- myQuery =
--     repository
--         & set owner "kowainik"
--         & set name  "stan"
--         >. (pullRequests
--             & set last 5
--             & set states [Closed, Open]
--             >. (nodes
--                ?. title
--                >. (author
--                    ?. login
--                   )
--                )
--            )

myQuery :: QueryGraphql
myQuery =
    repository
        id
--        ( set owner "kowainik"
--        . set name  "stan"
--        )
        []
--        [ pullRequests
--            ( set last 5
--            . set states [Closed, Open]
--            )
--            [ nodes
--               setTtitle
--               [ author setLogin
--               ]
--            ]
--
--        ]

-- TODO: replace with a proper AST
newtype QueryGraphql = QueryGraphql Text

repository :: (Repository -> Repository) -> [RepositoryPayload] -> QueryGraphql
repository _setRepository _payload = QueryGraphql ""

-- instance ToGraphQL Repositry where
--     toGraphQL Repository{..} = GrqphQLQuery $
--       "repository(" <> "owner: " <> rOwner <> ", name: " <> rName <> ") {" <>
--       <> toGraphQL rQuery
--       <> "}"

data Repository = Repository
    { repositoryOwner :: !Text
    , repositoryName  :: !Text
    }

emptyRepository :: Repository
emptyRepository = Repository
    { repositoryOwner = ""
    , repositoryName  = ""
    }

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
    { last   :: !Int
    , states :: ![State]
    , nodes  :: !a
--    , edges    :: !(Edge a)
--    , pageInfo :: !()
    }

data Edge a = Edge
    { edgeCursor :: !Text
    , edgeNode   :: !a
    }

data State
    = Open
    | Closed

data PullRequest = PullRequest
    { prTitle  :: !Text
    , prAuthor :: !Actor
    }

emptyPullRequest :: PullRequest
emptyPullRequest = PullRequest
    { prTitle = ""
    , prAuthor = emptyActor
    }

pullRequest :: (PullRequest -> PullRequest) -> RepositoryPayload
pullRequest setPullRequest = RepositoryPayload SRCPullRequests Connection
    { last = 0
    , states = []
    , nodes = setPullRequest emptyPullRequest
    }


newtype Actor = Actor
    { actorLogin :: Text
    }

emptyActor :: Actor
emptyActor = Actor
    { actorLogin = ""
    }
