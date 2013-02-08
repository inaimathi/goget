{-# LANGUAGE OverloadedStrings #-}
module Util ( resOk, res404, resError, resNO, resFile, resIxItems
            , resPlaceholder
            , serveStatic
            , extractParams, withPostParams ) where

import Data.String (fromString)
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS

import Data.IxSet (IxSet(..), Proxy(..), toAscList )

import Network.Wai
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Network.HTTP.Types (ok200, unauthorized401, status404)

import Control.Monad (sequence)
import Control.Monad.Trans.Resource (ResourceT)

import Model

type RES = ResourceT IO Response

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
  res <- requiredPostParams req paramNames
  case res of
    Just params -> fn params
    Nothing ->
      resError $ concat ["Need '", paramsList, "' parameters"]
      where paramsList = BS.unpack $ BS.intercalate "', '" paramNames

requiredPostParams :: Request -> [BS.ByteString] -> ResourceT IO (Maybe [String])
requiredPostParams request paramNames = do
  (params, _) <- parseRequestBody lbsBackEnd request
  return $ extractParams params paramNames

extractParams :: [(BS.ByteString, BS.ByteString)] -> [BS.ByteString] -> Maybe [String]
extractParams params paramNames =
  sequence $ map lookunpack paramNames
  where lookunpack n = do
          res <- lookup n params
          return $ BS.unpack res