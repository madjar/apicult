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
    r <- exampleGet "test_value"
    (r ^? key "args" % key "qs" % _String) `shouldBe` Just "test_value"

  it "post with json works" $ do
    r <- postJson "test_value"
    (r ^? key "json" % key "key" % _String) `shouldBe` Just "test_value"

  it "post with form works" $ do
    r <- postForm "test_value"
    (r ^? key "form" % key "key" % _String) `shouldBe` Just "test_value"

  it "redirect is an exception" $ do
    redirect
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