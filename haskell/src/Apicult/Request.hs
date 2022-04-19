{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Apicult.Request (makeRequestForEndpoint) where

import Apicult.Parse
import Control.Exception (ErrorCall (ErrorCall))
import Control.Monad.Catch (MonadThrow, throwM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Simple as H
import Relude.Extra (lookup)

makeRequestForEndpoint :: MonadThrow m => Endpoint -> m H.Request
makeRequestForEndpoint Endpoint {variables, request = request} =
  makeRequest request defaultArgs
  where
    defaultArgs = fromList . map (\Variable {name, defaultValue} -> (name, defaultValue)) $ variables

makeRequest :: MonadThrow m => Request -> Map Text Text -> m H.Request
makeRequest Request {method, url, querystring, headers, body = Body {bodyType, content}} args = do
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
    render = renderInterpolated args
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

renderInterpolated :: MonadThrow m => Map Text Text -> Interpolated -> m Text
renderInterpolated args (Interpolated parts) = fold <$> traverse render parts
  where
    render (Literal l) = return l
    render (Var v) =
      maybe
        (throwM (ErrorCall ("Missing argument: " <> toString v)))
        return
        (lookup v args)
