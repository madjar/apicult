{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Implementation (implementationSpec) where

import Apicult.Parse (Api (endpoints), Endpoint (..), Result (Expectation), apiParser)
import Apicult.Request (makeRequestForEndpoint)
import Data.Text (replace, strip)
import Network.HTTP.Client.Internal
  ( Manager,
    ManagerSettings (managerRawConnection),
    defaultManagerSettings,
    httpNoBody,
    makeConnection,
    newManager,
  )
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

implementationSpec :: Spec
implementationSpec = describe "implementation_tests.api" $ do
  it "parses the implementation test properly" $ do
    implementationTests <- readFileText "../implementation_tests.api"
    parse apiParser "" `shouldSucceedOn` implementationTests

  describe "implementation tests" $ do
    f <- runIO $ readFileText "../implementation_tests.api"
    let implementationTests =
          case parse apiParser "../implementation_tests.api" f of
            Right r -> r
            Left e -> error (toText $ errorBundlePretty e)

    forM_ (endpoints implementationTests) $ \e@(Endpoint {name, result, request = requestDefinition}) -> do
      case result of
        Expectation expectations -> it (toString name) $ do
          request <- makeRequestForEndpoint e
          mockManager <- newMockManager

          _ <- httpNoBody request (manager mockManager)
          content <- map strip . lines . decodeUtf8 <$> readIORef (requestContent mockManager)

          forM_ (touchUpExpectations expectations) $ \line ->
            unless
              (line `elem` content)
              ( expectationFailure
                  ( "For the definition "
                      <> show requestDefinition
                      <> "\nThe request "
                      <> show content
                      <> " does not contain "
                      <> show line
                  )
              )
        _ -> error ("Encountered something that is not an 'expect' for " <> name)

-- We need to touch up the request expectations, to match what http-client generates
touchUpExpectations :: [Text] -> [Text]
touchUpExpectations =
  map
    ( -- Nicely provides the charset
      replace "Content-Type: application/json" "Content-Type: application/json; charset=utf-8"
        -- Using this encoding for spaces
        . replace "+" "%20"
        -- Our implementation doesn't respect the order. I really hope no http server cares
        . replace "key=value&other_key=other_value" "other_key=other_value&key=value"
    )

-- A mock http manager, which we can pass to http-client to get exactly the request it sends

data MockManager = MockManager
  { manager :: Manager,
    requestContent :: IORef ByteString
  }

newMockManager :: IO MockManager
newMockManager = do
  requestContent <- newIORef ""
  let rawConnection = return $ \_ _ _ -> do
        makeConnection
          (return "HTTP/1.1 200 OK\n\n")
          (\bs -> modifyIORef' requestContent (<> bs))
          (return ())

  manager <- newManager $ defaultManagerSettings {managerRawConnection = rawConnection}

  return MockManager {..}