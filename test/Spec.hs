module Main (main) where

import GitHub (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
