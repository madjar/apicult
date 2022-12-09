module Main (main) where

import Test.Hspec (hspec)
import Test.HttpBin (httpBinSpec)
import Test.Implementation (implementationSpec)
import Test.TH (thSpec)

main :: IO ()
main = hspec $ do
  implementationSpec
  httpBinSpec
  thSpec
