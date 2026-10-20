module Main (main) where

import Munch (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
