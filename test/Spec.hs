module Main (main) where

import Test.Hspec (hspec)

import Test.Query (querySpecs)
import Test.Render (renderSpecs)


main :: IO ()
main = hspec $ do
    renderSpecs
    querySpecs
