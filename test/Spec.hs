module Main (main) where

import Test.Hspec (hspec)

import Test.Query (querySpecs)
import Test.Render (renderSpecs)

import qualified GitHub as GH


main :: IO ()
main = do
    token <- GH.getGitHubToken "GITHUB_TOKEN" >>= \case
        Nothing    -> error "No env variable 'GITHUB_TOKEN'"
        Just token -> pure token

    hspec $ do
        renderSpecs
        querySpecs token
