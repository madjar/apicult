{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.HttpBin where

import Apicult.TH
import Data.Aeson.Optics
import Network.HTTP.Client (HttpExceptionContent (StatusCodeException))
import Network.HTTP.Simple (HttpException (HttpExceptionRequest), getResponseStatusCode)
import Optics
import Test.Hspec

makeApi "test/httpbin.api"

httpBinSpec :: Spec
httpBinSpec = describe "Httpbin" $ do
  it "get works" $ do
    let client = newClient "ignored"
    r <- exampleGet client "test_value"
    (r ^? key "args" % key "qs" % _String) `shouldBe` Just "test_value"

  it "post with json works" $ do
    let client = newClient "ignored"
    r <- postJson client "test_value"
    (r ^? key "json" % key "key" % _String) `shouldBe` Just "test_value"

  it "post with form works" $ do
    let client = newClient "ignored"
    r <- postForm client "test_value"
    (r ^? key "form" % key "key" % _String) `shouldBe` Just "test_value"

  it "code generation with default value" $ do
    let client = newClient "ignored"
    r <- exampleGetWithDefaultValue client Nothing
    (r ^? key "args" % key "qs" % _String) `shouldBe` Just "some_value"

  it "redirect is an exception" $ do
    let client = newClient "ignored"
    redirect client
      `shouldThrow` anyRedirect

anyRedirect :: Selector HttpException
anyRedirect
  ( HttpExceptionRequest
      _
      (StatusCodeException response _)
    )
    | getResponseStatusCode response == 300 =
        True
anyRedirect _ = False