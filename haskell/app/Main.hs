module Main (main) where

import Apicult (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
