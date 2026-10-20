module Main (main) where

import Munch (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
