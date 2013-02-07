{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Default (def)
import Data.String (fromString)
import Data.Maybe (fromMaybe)
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vault as Vault

import Control.Monad.Trans  (liftIO)

import Data.Acid (AcidState, Update, Query, makeAcidic, openLocalState)
import Data.Acid.Advanced (update', query')
import Data.Acid.Local (createCheckpointAndClose)
import Data.IxSet (Indexable(..), IxSet(..), (@=), Proxy(..), getOne, ixFun, ixSet, insert, delete, toAscList, updateIx )

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
    
import Crypto.Scrypt (EncryptedPass(..), Pass(..), defaultParams, encryptPass, verifyPass)

import Model
import Util

---------- Synonyms for common complex types
type DB = AcidState GoGetDB
type RES = ResourceT IO Response
type LookupFN = (String -> ResourceT IO (Maybe String))
type InsertFN = (String -> String -> ResourceT IO ())

---------- HTTP Handlers
listItems :: DB -> Account -> RES
listItems db user = do
  resOk $ toAscList (Proxy :: Proxy ItemStatus) $ accountItems user

needItem :: DB -> Account -> Item -> RES
needItem db user item = do
  update' db $ ChangeItem user new
  resIxItems $ updateIx (itemName item) new (accountItems user) 
    where new = item { itemStatus = Need }

gotItem :: DB -> Account -> Item -> RES
gotItem db user item = do
  update' db $ ChangeItem user new
  resIxItems $ updateIx (itemName item) new (accountItems user)
    where new = item { itemStatus = Got }

deleteItem :: DB -> Account -> Item -> RES
deleteItem db user item = do
  update' db $ DeleteItem user item
  resIxItems $ delete item (accountItems user)

newItem :: DB -> Account -> String -> String -> Integer -> RES
newItem db user name comment count = do
  update' db $ NewItem user item
  resIxItems $ insert item (accountItems user)
  where item = Item { itemName=name, itemComment=comment, itemCount=count, itemStatus=Need }

changePassphrase :: DB -> Account -> String -> RES
changePassphrase db user newPassphrase = do
  new <- liftIO $ encryptPass defaultParams . Pass $ BS.pack newPassphrase
  update' db $ UpdateAccount $ user { accountPassphrase = unEncryptedPass new }
  resOk user

register :: DB -> InsertFN -> String -> String -> RES
register db sessionInsert name passphrase = do
  pass <- liftIO $ encryptPass defaultParams . Pass $ BS.pack passphrase
  existing <- query' db $ AccountByName name
  case existing of
    Nothing -> do
      acct <- update' db $ NewAccount name $ unEncryptedPass pass
      sessionInsert "user" name
      resOk acct
    _ -> resNO

login :: DB -> InsertFN -> String -> String -> RES
login db sessionInsert name passphrase = do 
  res <- query' db $ AccountByName name
  case res of
    Just user -> case verifyPass defaultParams (Pass $ BS.pack passphrase) pass of
      (True, _) -> do
        sessionInsert "user" $ accountName user
        resOk user
      _ -> resNO
      where pass = EncryptedPass $ accountPassphrase user
    _ -> resNO

---------- Server Setup
----- Request parsing/path dispatch
routes :: DB -> Vault.Key (Session (ResourceT IO) String String) -> Request -> RES
routes db session req = do
  let Just (sessionLookup, sessionInsert) = Vault.lookup session (vault req)
  user <- sessionLookup "user"
  case pathInfo req of
    ("app":rest) -> 
      loggedInRoutes db user rest req
    ("auth":rest) ->
      authRoutes db sessionLookup sessionInsert rest req
    ["static", subDir, fileName] -> 
      serveStatic subDir fileName
    [] -> 
      resFile "text/html" "static/index.html"
    ["favicon.ico"] -> res404
    _ -> res404

authRoutes :: DB ->  LookupFN -> InsertFN -> [Text.Text] -> Request -> RES
authRoutes db sLookup sInsert path req = do
  case path of
    ["login"] -> do
      res <- requiredPostParams req ["name", "passphrase"]
      case res of
        Just [name, passphrase] ->
          login db sInsert name passphrase
        _ -> resError "Need 'name', 'passphrase' parameters" 
    ["register"] ->
      res404
    _ ->
      res404

loggedInRoutes :: DB -> Maybe String -> [Text.Text] -> Request -> RES
loggedInRoutes db maybeUserName path req = do
  (params, _) <- parseRequestBody lbsBackEnd req
  case maybeUserName of
    Just name -> do
      maybeAccount <- query' db $ AccountByName name
      case maybeAccount of
        Just user -> case path of
          ("item":rest) -> 
            case lookup "itemName" params of
              Just itemName -> 
                withItemRoutes db user (BS.unpack itemName) rest req
              _ -> 
                resError "Need 'itemName' parameter"
          ["list"] -> 
            listItems db user
          ["new"] -> do
            res <- requiredPostParams req ["itemName", "comment", "count"]
            case res of
              Just [name, comment, count] -> 
                newItem db user name comment (read count :: Integer)
              _ -> 
                resError "Need 'itemName', 'comment', 'count' parameters"
          ["change-passphrase"] -> 
            case lookup "newPassphrase" params of
              Just newPass ->
                changePassphrase db user $ BS.unpack newPass
              _ -> resError "Need 'newPassphrase' parameter"
          _ -> 
            res404
        Nothing -> resError "Invalid user"
    Nothing -> resError "Not Logged In"

withItemRoutes :: DB -> Account -> String -> [Text.Text] -> Request -> RES
withItemRoutes db user itemName path req = do
  case getOne $ (accountItems user) @= itemName of
    Just item -> case path of
      ["need"] -> 
        needItem db user item
      ["got"] -> 
        gotItem db user item
      ["delete"] -> 
        deleteItem db user item
      _ -> res404
    Nothing -> res404

----- Server start
main = do
  session <- Vault.newKey
  store <- mapStore_
  bracket (openLocalState initialDB) (createCheckpointAndClose) 
    (\db -> run 3000 $ withSession store (fromString "SESSION") def session $ routes db session)