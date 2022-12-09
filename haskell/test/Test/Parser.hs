{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Parser (parserSpec) where

import Apicult.Parse
import Test.Hspec
import Text.Megaparsec (parse)

parserSpec :: Spec
parserSpec = describe "Parser" $ do
  it "records the definition as documentation" $ do
    let definition = "# Example get\n> http https://httpbin.org/get\n"
    let result = parse apiParser "" definition

    result `shouldSatisfy` isRight
    whenRight () result $ \(Api {endpoints = [Endpoint {doc = doc}]}) ->
      doc `shouldBe` definition
