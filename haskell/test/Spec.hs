module Main (main) where

import Test.Hspec (hspec)
import Test.Implementation (implementationSpec)

main :: IO ()
main = hspec $ do
  implementationSpec
