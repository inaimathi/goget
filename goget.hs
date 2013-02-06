{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Default (def)
import Data.String (fromString)
import Data.Maybe (fromJust)
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vault as Vault

import Data.Acid (AcidState, Update, Query, makeAcidic, openLocalState)
import Data.Acid.Local (createCheckpointAndClose)

import Network.Wai
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.Map (mapStore_)
import Network.Wai.Handler.Warp (run)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Network.HTTP.Types (ok200, unauthorized401, status404)

import Control.Exception (bracket)
import Control.Concurrent.Chan (Chan, newChan, dupChan, writeChan)
import Control.Monad.Trans.Resource (ResourceT)
    
import qualified Model as Model

---------- HTTP Handlers
needItem db user itemName = do  
  Model.changeItemStatus db user itemName Model.Need
  listItems db user

gotItem db user itemName = do
  Model.changeItemStatus db user itemName Model.Got
  listItems db user

newItem db user itemName comment count = do
  Model.insertItem db user itemName comment count
  listItems db user

listItems db user = do
  lst <- Model.listItems db user
  resOk lst

-- deleteItem db user itemName = do
--   Model.removeItem db user itemName
--   listItems db user

register db sessionInsert name passphrase = do
  account <- Model.register db name passphrase
  case account of
    Just acct -> do 
      u <- acct
      sessionInsert "user" name
      resOk u
    Nothing -> resNO

login db sessionInsert name passphrase = do 
  mmAccount <- Model.authenticate db name passphrase
  case mmAccount of
    Just (Just account) -> do
      sessionInsert "user" $ Model.accountName account
      resOk account
    _ -> resNO

---------- Server Setup
----- Request parsing/path dispatch
app db session req = do
  let Just (sessionLookup, sessionInsert) = Vault.lookup session (vault req)
  user <- sessionLookup "user"
  case pathInfo req of
----- Inventory handlers    
    ["list"] -> 
      listItems db $ fromJust user
    ["need"] -> do
      [itemName] <- postParams req ["itemName"]
      needItem db (fromJust user) itemName
    ["got"] -> do
      [itemName] <- postParams req ["itemName"]
      gotItem db (fromJust user) itemName
    ["new"] -> do
      [itemName, comment, count] <- postParams req ["itemName", "comment", "count"]
      newItem db (fromJust user) itemName comment (read count)
----- Login handlers
    ["login"] ->
      login db sessionInsert "Test" "iamtheverymodelofamodernmajorgeneral"
    ["register"] ->
      register db sessionInsert "Test 2" "a passphrase goes here"
----- Static/error handlers      
    ["static", subDir, fileName] -> 
      serveStatic subDir fileName
    [] -> 
      resFile "text/html" "static/index.html"
    ["favicon.ico"] -> res404
    _ -> res404

----- Server start
main = do
  session <- Vault.newKey
  store <- mapStore_
  bracket (openLocalState Model.initialDB) (createCheckpointAndClose) 
    (\db -> run 3000 $ withSession store (fromString "SESSION") def session $ app db session)

---------- Utility
resOk :: (ToJSON a) => a -> ResourceT IO Response
resOk body = return $ responseLBS ok200 [] $ encode body

resNO :: ResourceT IO Response
resNO = return $ responseLBS unauthorized401 [] $ fromString "NO -_-"

res404 :: ResourceT IO Response
res404 = return $ responseLBS status404 [] $ fromString "Not Found"

resFile :: BS.ByteString -> FilePath -> ResourceT IO Response
resFile contentType filename = return $ ResponseFile ok200 [("Content-Type", contentType)] filename Nothing

postParams :: Request -> [BS.ByteString] -> ResourceT IO [String]
postParams request paramNames = do
  (params, _) <- parseRequestBody lbsBackEnd request
  let fromParam name = BS.unpack $ fromJust $ lookup name params
      rec [] acc = reverse acc
      rec (name:rest) acc = rec rest $ (fromParam name):acc
  return $ rec paramNames []

serveStatic :: Text.Text -> Text.Text -> ResourceT IO Response
serveStatic subDir fName = 
  case sub of
    "js" -> serve "text/javascript"
    "css" -> serve "text/css"
    "img" -> serve "image/png"
    _ -> res404
  where serve mimeType = resFile mimeType $ concat ["static/", sub, "/", Text.unpack fName]
        sub = Text.unpack subDir