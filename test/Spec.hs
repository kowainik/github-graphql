module Main (main) where

import Test.Hspec (hspec)

import Test.Render (renderSpecs)


main :: IO ()
main = hspec
    renderSpecs
