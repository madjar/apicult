{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Apicult.TH (makeApi, makeApi') where

import Apicult.Parse
import Apicult.Request
import Data.Aeson qualified as Aeson
import Data.List (partition)
import Data.Text qualified as Text
import Language.Haskell.TH
import Language.Haskell.TH.Name.CamelCase (VarName (varName), toVarName, varCamelcaseName)
import Network.HTTP.Simple qualified as H
import Relude.Extra (lookup, member)
import Prelude hiding (Type)

makeApi :: FilePath -> Q [Dec]
makeApi definitionFile = do
  api <- liftIO $ parseApi definitionFile
  makeApi' api False

makeApi' :: Api -> Bool -> Q [Dec]
makeApi' Api {globals, endpoints} isIO = do
  let globalVariables :: [Text] -- XXX duplicated
      globalVariables = map (\Variable {name} -> name) globals
  (clientDecs, maybeClientType) <- clientQ globals
  let maybeClientCon = maybeClientType

  functions <- foldFor endpoints $ \e@Endpoint {name, variables, request, result = _, doc} -> do
    allArgs <- executingStateT [] (makeRequest request (\key -> modify (key :) >> return ""))

    -- First, separate args that are in Client, and args specific to this function
    let (clientArgs, args) = partition (`elem` globalVariables) allArgs

    let functionName = varName . varCamelcaseName . toString $ name
        argsWithDefaults = variablesWithDefaults variables
        clientRecPat arg = let argName = varName . toVarName . toString $ arg in fieldPat argName (varP argName)
        clientPat = maybe [] (\n -> [recP n (map clientRecPat clientArgs)]) maybeClientCon
        functionArgs =
          clientPat
            ++ map (varP . varName . toVarName . toString) args

    let argTypes =
          maybe [] (\n -> [conT n]) maybeClientType
            ++ map (\arg -> if member arg argsWithDefaults then [t|Maybe Text|] else [t|Text|]) args
        type_ = functionType argTypes [t|IO Aeson.Value|]
    signature <- sigD functionName type_
    let withDecDoc' = if isIO then \_ dec -> dec else withDecDoc
    function <-
      withDecDoc' (Text.unpack ("@\n" <> doc <> "@")) $
        funD
          functionName
          [ clause
              functionArgs
              ( normalB
                  [|
                    do
                      req <- $(requestForEndpointQ e)
                      resp <- H.httpJSON req
                      return (H.getResponseBody resp) :: IO Aeson.Value
                    |]
              )
              []
          ]
    return [signature, function]

  return (clientDecs <> functions)

-- | This is `flip foldMap`, except template-haskell doesn't have `Monoid Q` in ghc 8.10
foldFor :: [a] -> (a -> Q [Dec]) -> Q [Dec]
foldFor l f = concat <$> forM l f

clientQ :: [Variable] -> Q ([Dec], Maybe Name)
clientQ globals = do
  let globalVariables :: [Text]
      globalVariables = map (\Variable {name} -> name) globals
  if null globalVariables
    then return ([], Nothing)
    else do
      let typeClientName = mkName "Client"
          conClientName = typeClientName
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
      newClientSignature <- sigD newClientName (functionType (map (const [t|Text|]) globalVariables) (conT typeClientName))
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
      return ([clientDef, newClientSignature, newClientFunction], Just typeClientName)

-- | Given a list of argument types and a return type, returns the type of a function
functionType :: [Q Type] -> Q Type -> Q Type
functionType args ret = do
  foldr funcT ret args
  where
    funcT a = appT (appT arrowT a)

-- | Generate a request for an endpoint. Right now, we enumerate the variable used and generate a Map,
-- which is not great.
requestForEndpointQ :: Endpoint -> Q Exp
requestForEndpointQ endpoint@Endpoint {request, variables} = do
  args <- executingStateT [] (makeRequest request (\key -> modify (key :) >> return ""))
  let argsWithDefaults = variablesWithDefaults variables
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

  [|makeRequestForEndpoint endpoint (fromList $argMap)|]