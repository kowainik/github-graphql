module Test.Render
    ( renderSpecs
    ) where

import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

import GitHub (exampleQuery, repositoryToAst)
import GitHub.Render (renderTopQuery)

import qualified Data.Text as T


renderSpecs :: Spec
renderSpecs = describe "Rendering" $
    it "should render an example query" $
        renderTopQuery (repositoryToAst exampleQuery) `shouldBe` exampleRendered

exampleRendered :: Text
exampleRendered = T.unlines
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
