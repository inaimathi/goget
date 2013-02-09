{-# LANGUAGE OverloadedStrings #-}
module Util ( resOk, res404, resError, resNO, resFile, resIxItems, serveStatic
            , resPlaceholder
            , extractOptional, withParams, withPostParams 
            , maybeRead) where

import Data.String (fromString)
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS

import Data.IxSet (IxSet(..), Proxy(..), toAscList )

import Network.Wai
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Network.HTTP.Types (ok200, unauthorized401, status404)

import Control.Monad (sequence, liftM)
import Control.Monad.Trans.Resource (ResourceT)

import TypeSynonyms
import Model

resIxItems :: IxSet Item -> RES
resIxItems body = resOk $ toAscList (Proxy :: Proxy ItemStatus) $ body

resOk :: (ToJSON a) => a -> RES
resOk body = return $ responseLBS ok200 [] $ encode body

resNO :: RES
resNO = resError "NO -_-"

res404 :: RES
res404 = return $ responseLBS status404 [] $ fromString "Not Found"

resPlaceholder :: RES
resPlaceholder = return $ responseLBS status404 [] $ fromString "Not implemented yet"

resError :: String -> RES
resError message = return $ responseLBS unauthorized401 [] $ fromString message

resFile :: BS.ByteString -> FilePath -> RES
resFile contentType filename = return $ ResponseFile ok200 [("Content-Type", contentType)] filename Nothing

serveStatic :: Text.Text -> Text.Text -> RES
serveStatic subDir fName = 
  case sub of
    "js" -> serve "text/javascript"
    "css" -> serve "text/css"
    "img" -> serve "image/png"
    _ -> res404
  where serve mimeType = resFile mimeType $ concat ["static/", sub, "/", Text.unpack fName]
        sub = Text.unpack subDir

withPostParams :: Request -> [BS.ByteString] -> ([String] -> RES) -> RES
withPostParams req paramNames fn = do
  (params, _) <- parseRequestBody lbsBackEnd req
  withParams params paramNames fn

withParams :: BSAssoc -> [BS.ByteString] -> ([String] -> RES) -> RES
withParams params paramNames fn = 
  case extractParams params paramNames of
    Just paramVals -> 
      fn paramVals
    Nothing ->
      resError $ concat ["Need '", paramsList, "' parameters"]
      where paramsList = BS.unpack $ BS.intercalate "', '" paramNames

extractOptional :: BSAssoc -> [BS.ByteString] -> [Maybe String]
extractOptional  params paramNames = map lookunpack paramNames
  where lookunpack k = do
          res <- lookup k params
          return $ BS.unpack res

extractParams :: BSAssoc -> [BS.ByteString] -> Maybe [String]
extractParams params paramNames = do
  res <- allLookups params paramNames
  return $ map BS.unpack res

maybeRead :: Read a => Maybe String -> Maybe a
maybeRead str = do
  res <- str
  return $ read res

allLookups :: Eq a => [(a, a)] -> [a] -> Maybe [a]
allLookups assoc keys = sequence $ map (\k -> lookup k assoc) keys