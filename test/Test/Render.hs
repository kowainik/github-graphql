module Test.Render
    ( renderSpecs
    ) where

import Data.Function ((&))
import Data.Text (Text)
import Prolens (set)
import Test.Hspec (Spec, describe, it, shouldBe)

import GitHub (createIssueToAst, exampleQuery, repositoryToAst)
import GitHub.Render (renderTopMutation, renderTopQuery)

import qualified Data.Text as T

import qualified GitHub as GH


renderSpecs :: Spec
renderSpecs = describe "Rendering" $ do
    it "should render an example query" $
        renderTopQuery (repositoryToAst exampleQuery) `shouldBe` queryRendered
    it "should render an example mutation" $
        renderTopMutation (createIssueToAst exampleMutation) `shouldBe` mutationRendered

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
    , "  }"
    , "}"
    ]

exampleMutation :: GH.CreateIssue
exampleMutation = GH.CreateIssue
    ( GH.defCreateIssueInput
    & set GH.repositoryIdL (GH.Id "MDEwOlJlcG9zaXRvcnkyOTA1MDA2MzI=")
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
