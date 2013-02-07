{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
 
module Model ( initialDB
             , listAccounts, changePassword
             , GoGetDB(..), Account(..), Item(..), ItemStatus(..)
             , NewAccount(..), UpdateAccount(..), AccountByName(..), GetAccounts(..)
             , NewItem(..), DeleteItem(..), ChangeItem(..) ) where

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Control.Monad.Trans  (liftIO)
import Control.Monad.IO.Class (MonadIO)

import Data.Acid (AcidState, Update, Query, makeAcidic, openLocalState)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Acid.Advanced (update', query')
import Data.Data (Data, Typeable)
import Data.IxSet (Indexable(..), IxSet(..), (@=), Proxy(..), getOne, ixFun, ixSet, insert, delete, toAscList, updateIx )
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as Text
import Data.ByteString.Char8 (ByteString, pack)
import Data.Maybe
import Data.Aeson

import Crypto.Scrypt (EncryptedPass(..), Pass(..), defaultParams, encryptPass, verifyPass)

---------- Base types (for IxSet and Account components)
newtype AccountId = AccountId { unAccountId :: Integer } deriving (Eq, Ord, Data, Show, Enum, Typeable, SafeCopy)

---------- Item-Related types
data ItemStatus = Need | Got deriving (Eq, Ord, Data, Enum, Read, Show, Typeable)
deriveSafeCopy 0 'base ''ItemStatus

data Item = Item { itemName :: String, itemComment :: String, itemStatus :: ItemStatus, itemCount :: Integer } deriving (Eq, Ord, Show, Data, Typeable)
deriveSafeCopy 0 'base ''Item
instance Indexable Item where
  empty = ixSet [ ixFun $ (:[]) . itemName
                , ixFun $ (:[]) . itemStatus
                , ixFun $ (:[]) . itemCount
                ]
instance ToJSON Item where
  toJSON (Item name comment status count) = object [ "name" .= name
                                                   , "comment" .= comment
                                                   , "status" .= show status
                                                   , "count" .= count 
                                                   ]

---------- Account
data Account = Account { accountId :: AccountId
                       , accountName :: String 
                       , accountPassphrase :: ByteString
                       , accountItems :: IxSet Item
                       } deriving (Eq, Show, Data, Typeable) 

instance Ord Account where
  a `compare` b = (accountId a) `compare` (accountId b)

deriveSafeCopy 0 'base ''Account
instance Indexable Account where
  empty = ixSet [ ixFun $ (:[]) . accountId
                , ixFun $ (:[]) . accountName 
                ]

instance ToJSON Account where
  toJSON (Account id name _ items) = object [ "id" .= unAccountId id
                                            , "name" .= name
                                            , "items" .= toAscList (Proxy :: Proxy ItemStatus) items]

---------- DB root type
  --- This is declared so that acid-state has a top level element to store
data GoGetDB = GoGetDB { nextAccountId :: AccountId, accounts :: IxSet Account
                       } deriving (Show, Data, Typeable)
deriveSafeCopy 0 'base ''GoGetDB

initialDB :: GoGetDB
initialDB = GoGetDB { nextAccountId = AccountId 0, accounts = empty }

---------- Insertion Functions
newAccount :: String -> ByteString -> Update GoGetDB Account
newAccount name passphrase = do
  db@GoGetDB{..} <- get
  let account = Account { accountId = nextAccountId
                        , accountName = name
                        , accountPassphrase = passphrase
                        , accountItems = empty
                        }
  put $ db { nextAccountId = succ nextAccountId
           , accounts = insert account accounts 
           }
  return account

deleteItem :: Account -> Item -> Update GoGetDB ()
deleteItem acct item = do
  db@GoGetDB{..} <- get
  put $ db { accounts = updateIx (accountId acct) removed accounts }
    where removed = acct { accountItems = delete item (accountItems acct)}

newItem :: Account -> Item -> Update GoGetDB ()
newItem acct item = do
  db@GoGetDB{..} <- get
  put $ db { accounts = updateIx (accountId acct) added accounts }
    where added = acct { accountItems = insert item (accountItems acct) }

changeItem :: Account -> Item -> Update GoGetDB ()
changeItem acct item = do
  db@GoGetDB{..} <- get
  put $ db { accounts = updateIx (accountId acct) changed accounts }
    where changed = acct { accountItems = updateIx (itemName item) item (accountItems acct)}

updateAccount :: Account -> Update GoGetDB ()
updateAccount u = do
  db@GoGetDB{..} <- get
  put $ db { accounts = updateIx (accountId u) u accounts }

---------- Query Functions
getAccounts :: Query GoGetDB [Account]
getAccounts = do
  GoGetDB{..} <- ask
  return $ toAscList (Proxy :: Proxy AccountId) accounts

getAccount :: (Typeable a) => a -> Query GoGetDB (Maybe Account)
getAccount ix = do
  GoGetDB{..} <- ask
  return $ getOne $ accounts @= ix

accountByName :: String -> Query GoGetDB (Maybe Account)
accountByName name = getAccount name

makeAcidic ''GoGetDB [ 'newAccount, 'newItem, 'deleteItem, 'changeItem, 'updateAccount, 'accountByName, 'getAccounts ]

---------- External API
----- Utility
withAccount :: MonadIO m => AcidState GoGetDB -> String -> (Account -> a) -> m (Maybe a)
withAccount db name fn = do
  maybeAccount <- query' db $ AccountByName name
  return $ do acct <- maybeAccount
              return $ fn acct

----- Account-related
changePassword db name passphrase = do
  withAccount db name change
  where change account = do
          pass <- liftIO $ encryptPass defaultParams . Pass $ pack passphrase
          update' db $ UpdateAccount $ account { accountPassphrase = unEncryptedPass pass }

listAccounts db = query' db $ GetAccounts
