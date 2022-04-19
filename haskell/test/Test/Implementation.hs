module Test.Implementation (implementationSpec) where

import Apicult.Parse (apiParser)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

implementationSpec :: Spec
implementationSpec = it "parses the implementation test properly" $ do
  implementationTests <- readFileText "../implementation_tests.api"
  parse apiParser "" `shouldSucceedOn` implementationTests