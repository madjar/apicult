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
import Relude.Extra (lookup, member)
import Prelude hiding (Type)

makeApi :: FilePath -> Q [Dec]
makeApi definitionFile = do
  Api {globals, endpoints} <- liftIO $ parseApi definitionFile
  let globalVariables :: [Text]
      globalVariables = map (\Variable {name} -> name) globals

  clientStuff <- memptyIfTrue (null globalVariables) $ do
    typeClientName <- newName "Client" :: Q Name
    conClientName <- newName "Client"
    let recordField =
          map
            ( \var ->
                varBangType
                  (varName . toVarName . toString $ var)
                  (bangType (return (Bang NoSourceUnpackedness SourceStrict)) [t|Text|])
            )
            globalVariables
    clientDef <- dataD mempty typeClientName [] Nothing [recC conClientName recordField] []
    -- TODO newClient that only takes non-defaults
    newClientName <- newName "newClient"
    let argsNames = map (varName . toVarName . toString) globalVariables
    newClientFunction <-
      funD
        newClientName
        [ clause
            (map varP argsNames) -- global variables without default
            ( normalB
                ( recConE
                    conClientName
                    (map (\name -> return (name, VarE name)) argsNames) -- attributes of Client
                )
            )
            []
        ]
    return [clientDef, newClientFunction]

  functions <- foldFor endpoints $ \e@Endpoint {name, variables, request, result = _} -> do
    args <- executingStateT [] (makeRequest request (\key -> modify (key :) >> return ""))

    -- TODO use client
    let functionName = varName . varCamelcaseName . toString $ name
        argsWithDefaults = variablesWithDefaults variables
        functionArgs = map (varP . varName . toVarName . toString) args
        argMap =
          listE $
            map
              ( \arg ->
                  let varExpr = varE . varName . toVarName . toString $ arg
                      argValue = case lookup arg argsWithDefaults of
                        Just defV -> [|fromMaybe defV $varExpr|]
                        Nothing -> varExpr
                   in [|(arg, $argValue)|]
              )
              args

    let argTypes = flip map args $ \arg -> if member arg argsWithDefaults then [t|Maybe Text|] else [t|Text|]
        type_ = functionType argTypes [t|IO Aeson.Value|]
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

  return (clientStuff <> functions)

-- | This is `flip foldMap`, except template-haskell doesn't have `Monoid Q` in ghc 8.10
foldFor :: [a] -> (a -> Q [Dec]) -> Q [Dec]
foldFor l f = concat <$> forM l f


-- | Given a list of argument types and a return type, returns the type of a function
functionType :: [Q Type] -> Q Type -> Q Type
functionType args ret = do
  foldr funcT ret args
  where funcT a = appT (appT arrowT a)
