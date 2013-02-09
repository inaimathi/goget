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
type BSAssoc = [(BS.ByteString, BS.ByteString)]

---------- HTTP Handlers
----- Item Related
listItems :: DB -> Account -> RES
listItems db user = do
  resIxItems  $ accountItems user

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

editItem :: DB -> Account -> Item -> Maybe String -> Maybe String -> RES
editItem db user item newComment newCount = do
  update' db $ ChangeItem user new
  resIxItems $ updateIx (itemName item) new (accountItems user)
    where new = item { itemComment = comment, itemCount = count }
          comment = fromMaybe (itemComment item) newComment
          count = fromMaybe (itemCount item) (maybeRead newCount :: Maybe Integer)

deleteItem :: DB -> Account -> Item -> RES
deleteItem db user item = do
  update' db $ DeleteItem user item
  resIxItems $ delete item (accountItems user)

newItem :: DB -> Account -> String -> String -> Integer -> RES
newItem db user name comment count = do
  update' db $ NewItem user item
  resIxItems $ insert item (accountItems user)
  where item = Item { itemName=name, itemComment=comment, itemCount=count, itemStatus=Need }

----- Account Related
changePassphrase :: DB -> Account -> String -> RES
changePassphrase db user newPassphrase = do
  new <- liftIO . encryptPass defaultParams . Pass $ BS.pack newPassphrase
  update' db . UpdateAccount $ user { accountPassphrase = unEncryptedPass new }
  resOk user

register :: DB -> InsertFN -> String -> String -> RES
register db sessionInsert name passphrase = do
  pass <- liftIO . encryptPass defaultParams . Pass $ BS.pack passphrase
  existing <- query' db $ AccountByName name
  case existing of
    Nothing -> do
      acct <- update' db . NewAccount name $ unEncryptedPass pass
      sessionInsert "user" name
      resOk acct
    _ -> resError "User already exists"

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
    ["favicon.ico"] -> 
      resPlaceholder
    _ -> res404

authRoutes :: DB ->  LookupFN -> InsertFN -> [Text.Text] -> Request -> RES
authRoutes db sLookup sInsert path req = do
  withPostParams req ["name", "passphrase"] route
  where route [name, pass] = 
          case path of
            ["login"] -> 
              login db sInsert name pass
            ["register"] -> 
              case pass of
                "" -> resError "At least pick a non-empty passphrase"
                _  -> register db sInsert name pass
            _ -> res404

loggedInRoutes :: DB -> Maybe String -> [Text.Text] -> Request -> RES
loggedInRoutes db maybeUserName path req = do
  (params, _) <- parseRequestBody lbsBackEnd req
  case maybeUserName of
    Just name -> do
      maybeAccount <- query' db $ AccountByName name
      case maybeAccount of
        Just user -> case path of
          ("item":rest) -> 
            withParams params ["itemName"] route
            where route [itemName] = itemRoutes db user itemName rest params
          ["list"] -> 
            listItems db user
          ["new"] -> 
            withParams params ["itemName", "comment", "count"] new
            where new [name, comment, count] = newItem db user name comment (read count :: Integer)
          ["change-passphrase"] -> 
            withParams params ["newPassphrase"] change
            where change [newPass] = changePassphrase db user newPass
          _ -> res404
        Nothing -> resError "Invalid user"
    Nothing -> resError "Not Logged In"

itemRoutes :: DB -> Account -> String -> [Text.Text] -> BSAssoc -> RES
itemRoutes db user itemName path params = do
  case getOne $ (accountItems user) @= itemName of
    Just item -> case path of
      ["need"] -> 
        needItem db user item
      ["got"] -> 
        gotItem db user item
      ["delete"] -> 
        deleteItem db user item
      ["edit"] ->
        edit $ extractOptional params ["comment", "count"]
        where edit [comment, count] = editItem db user item comment count
      _ -> res404
    Nothing -> resError "Invalid item"

----- Server start
main = do
  session <- Vault.newKey
  store <- mapStore_
  bracket (openLocalState initialDB) (createCheckpointAndClose) 
    (\db -> run 3000 . withSession store (fromString "SESSION") def session $ routes db session)