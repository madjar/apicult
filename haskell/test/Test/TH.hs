{-# LANGUAGE DuplicateRecordFields #-}

module Test.TH (thSpec) where

import Apicult.Parse
import Apicult.TH
import Language.Haskell.TH
import Test.Hspec

emptyApi :: Api
emptyApi = Api {globals = [], endpoints = []}

variable :: Variable
variable = Variable {name = "var", defaultValue = Nothing}

endpointWithVar :: Endpoint
endpointWithVar =
  Endpoint
    { name = "get",
      doc = "",
      variables = [],
      request = Request {method = Get, url = Interpolated [Literal "http://example.com/", Var "var"], querystring = [], headers = [], body = Body {bodyType = Json, content = []}},
      result = NoResult
    }

runIt :: Api -> IO String
runIt api = do
  decs <- runQ $ makeApi' api True
  return $ pprint decs

thSpec :: Spec
thSpec = describe "TH" $ do
  it "doesn't use a client if there's no global variables" $ do
    code <- runIt emptyApi {endpoints = [endpointWithVar]}
    code `shouldContain` "get var = "
  it "creates a client if there are global variables" $ do
    code <- runIt emptyApi {globals = [variable]}
    code `shouldContain` "data Client"
  it "uses the client if it exists" $ do
    code <- runIt emptyApi {globals = [variable], endpoints = [endpointWithVar]}
    code `shouldContain` "get (Client {var = var}) ="
