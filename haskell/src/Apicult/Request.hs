{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Apicult.Request (makeRequestForEndpoint, makeRequest) where

import Apicult.Parse
import Control.Exception (ErrorCall (ErrorCall))
import Control.Monad.Catch (MonadThrow, throwM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Simple as H
import Relude.Extra (lookup)

-- | Create a request from an endpoint definition, use for dynamic clients
makeRequestForEndpoint :: MonadThrow m => Endpoint -> Map Text Text -> m H.Request
makeRequestForEndpoint Endpoint {variables, request = request} args =
  makeRequest request lookupArgs
  where
    defaultArgs = fromList . map (\Variable {name, defaultValue} -> (name, defaultValue)) $ variables
    fullArgs = args <> defaultArgs
    lookupArgs v =
      maybe
        (throwM (ErrorCall ("Missing argument: " <> toString v)))
        return
        (lookup v fullArgs)

-- | Make a request from the request definition. Takes function that maps variables to their values (this can be used to get a list of variables).
makeRequest :: MonadThrow m => Request -> (Text -> m Text) -> m H.Request
makeRequest Request {method, url, querystring, headers, body = Body {bodyType, content}} lookupArgs = do
  rUrl <- render url
  rQueryString <- renderQS <$> renderPairs querystring
  rHeaders <- renderHeaders <$> renderPairs headers
  rBody <- renderPairs content

  baseRequest <- H.parseRequestThrow (toString rUrl)
  return
    ( setBody rBody
        . H.setRequestHeaders rHeaders
        . H.addToRequestQueryString rQueryString
        . H.setRequestMethod (renderMethod method)
        $ baseRequest
    )
  where
    renderPairs =
      traverse
        ( \(Pair k v) ->
            do
              rv <- render v
              return (k, rv)
        )
    render = renderInterpolated lookupArgs
    renderMethod Get = "GET"
    renderMethod Post = "POST"
    renderQS = map (bimap encodeUtf8 (Just . encodeUtf8))
    renderHeaders = map (bimap (CI.mk . encodeUtf8) encodeUtf8)
    setBody :: [(Text, Text)] -> H.Request -> H.Request
    setBody rBody =
      if not (null rBody)
        then case bodyType of
          Form -> H.setRequestBodyURLEncoded (map (bimap encodeUtf8 encodeUtf8) rBody)
          Json -> H.setRequestBodyJSON (Aeson.object . map (bimap Aeson.fromText Aeson.toJSON) $ rBody)
        else id

-- NOTE: if I wanted to be fancy, I could have the rendering code also serve to collect the name of variables, through some fancy Applicative
renderInterpolated :: Applicative m => (Text -> m Text) -> Interpolated -> m Text
renderInterpolated lookupArgs (Interpolated parts) = fold <$> traverse render parts
  where
    render (Literal l) = pure l
    render (Var v) = lookupArgs v
