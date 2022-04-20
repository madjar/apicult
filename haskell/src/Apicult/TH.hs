{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Apicult.TH (makeApi) where

import Apicult.Parse
import Apicult.Request
import qualified Data.Aeson as Aeson
import Language.Haskell.TH
import Language.Haskell.TH.Name.CamelCase (VarName (varName), toVarName, varCamelcaseName)
import qualified Network.HTTP.Simple as H
import Prelude hiding (Type)

makeApi :: FilePath -> Q [Dec]
makeApi definitionFile = do
  -- TODO use globals
  Api {globals = _, endpoints} <- liftIO $ parseApi definitionFile
  foldFor endpoints $ \e@Endpoint {name, variables = _, request, result = _} -> do
    args <- executingStateT [] (makeRequest request (\key -> modify (key :) >> return ""))

    let functionName = varName . varCamelcaseName . toString $ name
        -- TODO handle default value
        functionArgs = map (varP . varName . toVarName . toString) args
        argMap = listE $ map (\n -> [|(n, $(varE . varName . toVarName . toString $ n))|]) args

    let type_ = functionType ([t|Text|] <$ args) [t|IO Aeson.Value|]
    signature <- sigD functionName type_
    function <-
      funD
        functionName
        [ clause
            functionArgs
            ( normalB
                [|
                  do
                    req <- makeRequestForEndpoint e (fromList $argMap)
                    resp <- H.httpJSON req
                    return (H.getResponseBody resp) :: IO Aeson.Value
                  |]
            )
            []
        ]
    return [signature, function]

-- | This is `flip foldMap`, except template-haskell doesn't have `Monoid Q` in ghc 8.10
foldFor :: [a] -> (a -> Q [Dec]) -> Q [Dec]
foldFor l f = concat <$> forM l f

functionType :: [Q Type] -> Q Type -> Q Type
functionType args ret = do
  args' <- sequence args
  ret' <- ret
  return (foldr (AppT . AppT ArrowT) ret' args')