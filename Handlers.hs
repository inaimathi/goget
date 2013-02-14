module Handlers ( listItems, needItem, gotItem, editItem, deleteItem, newItem
                , changePassphrase, register, login ) where

import Control.Monad.Trans  (liftIO)

import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BS

import Data.Acid (AcidState, Update, Query, makeAcidic, openLocalState)
import Data.Acid.Advanced (update', query')
import Data.Acid.Local (createCheckpointAndClose)
import Data.IxSet (Indexable(..), IxSet(..), (@=), Proxy(..), getOne, ixFun, ixSet, insert, delete, toAscList, updateIx )

import Crypto.Scrypt (EncryptedPass(..), Pass(..), defaultParams, encryptPass, verifyPass)

import TypeSynonyms
import Util
import Model


---------- HTTP Handlers
----- Item Related
listItems :: DB -> Account -> RES
listItems db user = do
  resIxItems  $ accountItems user

needItem :: DB -> Account -> Item -> RES
needItem db user item = updateItem db user $ item { itemStatus = Need }

gotItem :: DB -> Account -> Item -> RES
gotItem db user item = updateItem db user $ item { itemStatus = Got }

commentItem :: DB -> Account -> Item -> String -> RES
commentItem db user item newComment = updateItem db user $ item { itemComment = newComment }

countItem :: DB -> Account -> Item -> Integer -> RES
countItem db user item newCount = updateItem db user $ item { itemCount = newCount }

editItem :: DB -> Account -> Item -> Maybe String -> Maybe String -> RES
editItem db user item newComment newCount = updateItem db user new
  where new = item { itemComment = comment, itemCount = count }
        comment = fromMaybe (itemComment item) newComment
        count = fromMaybe (itemCount item) (maybeRead newCount :: Maybe Integer)

deleteItem :: DB -> Account -> Item -> RES
deleteItem db user item = do
  update' db $ DeleteItem user item
  resIxItems $ delete item (accountItems user)

updateItem :: DB -> Account -> Item -> RES
updateItem db user newItem = do
  update' db $ ChangeItem user newItem
  resIxItems $ updateIx (itemName newItem) newItem (accountItems user)

newItem :: DB -> Account -> String -> String -> Integer -> RES
newItem db user name comment count =
  case getOne $ (accountItems user) @= name of
    Just item -> needItem db user item
    Nothing -> do
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