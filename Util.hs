{-# LANGUAGE OverloadedStrings #-}
module Util ( resOk, res404, resError, resNO, resFile, resIxItems
            , serveStatic
            , requiredPostParams ) where

import Data.String (fromString)
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS

import Data.IxSet (IxSet(..), Proxy(..), toAscList )

import Network.Wai
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Network.HTTP.Types (ok200, unauthorized401, status404)

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

requiredPostParams :: Request -> [BS.ByteString] -> ResourceT IO (Maybe [String])
requiredPostParams request paramNames = do
  (params, _) <- parseRequestBody lbsBackEnd request
  let fromParam name = do 
        res <- lookup name params
        return $ BS.unpack res
      rec [] acc = Just $ reverse acc
      rec (name:rest) acc = do 
        res <- fromParam name
        rec rest $ res:acc
  return $ rec paramNames []