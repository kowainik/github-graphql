module Main (main) where

import GithubGraphql (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
