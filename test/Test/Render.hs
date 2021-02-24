module Test.Render
    ( renderSpecs
    ) where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Prolens (set)
import Test.Hspec (Spec, describe, it, shouldBe)

import GitHub (createIssueToAst, repositoryToAst)
import GitHub.Render (renderTopMutation, renderTopQuery)

import Test.Data (githubGraphqlRepositoryId)

import qualified Data.Text as T

import qualified GitHub as GH


renderSpecs :: Spec
renderSpecs = describe "Rendering" $ do
    it "should render an example query" $
        renderTopQuery (repositoryToAst exampleQuery) `shouldBe` queryRendered
    it "should render an example mutation" $
        renderTopMutation (createIssueToAst exampleMutation) `shouldBe` mutationRendered

exampleQuery :: GH.Repository
exampleQuery = GH.repository
    ( GH.defRepositoryArgs
    & set GH.ownerL "kowainik"
    & set GH.nameL  "hit-on"
    )
    $ GH.issues
        ( GH.defIssuesArgs
        & set GH.lastL 3
        & set GH.statesL (GH.one GH.Open)
        )
        (GH.one $ GH.nodes $
           GH.title :|
           [GH.author $ GH.one GH.login]
        )
    :|
    [ GH.pullRequests
        ( GH.defPullRequestsArgs
        & set GH.lastL 3
        & set GH.statesL (GH.one GH.Open)
        )
        (GH.one $ GH.nodes $
           GH.title :|
           [GH.author $ GH.one GH.login]
        )
    , GH.RepositoryId
    ]

queryRendered :: Text
queryRendered = T.unlines
    [ "query {"
    , "  repository(owner: \"kowainik\", name: \"hit-on\") {"
    , "    issues(last: 3, states: [OPEN]) {"
    , "      nodes {"
    , "        title"
    , "        author {"
    , "          login"
    , "        }"
    , "      }"
    , "    }"
    , "    pullRequests(last: 3, states: [OPEN]) {"
    , "      nodes {"
    , "        title"
    , "        author {"
    , "          login"
    , "        }"
    , "      }"
    , "    }"
    , "    id"
    , "  }"
    , "}"
    ]

exampleMutation :: GH.CreateIssue
exampleMutation = GH.CreateIssue
    ( GH.defCreateIssueInput
    & set GH.repositoryIdL githubGraphqlRepositoryId
    & set GH.titleL "Test title"
    )
    [ GH.title
    , GH.author $ GH.one GH.login
    ]

mutationRendered :: Text
mutationRendered = T.unlines
    [ "mutation {"
    , "  createIssue(input: {repositoryId: \"MDEwOlJlcG9zaXRvcnkyOTA1MDA2MzI=\", title: \"Test title\"}) {"
    , "    issue {"
    , "      title"
    , "      author {"
    , "        login"
    , "      }"
    , "    }"
    , "  }"
    , "}"
    ]
