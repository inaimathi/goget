module TypeSynonyms where

import Data.Acid (AcidState)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vault as Vault

import Network.Wai
import Network.Wai.Session (Session)

import Control.Monad.Trans.Resource (ResourceT)

import Model

type DB = AcidState GoGetDB
type RES = ResourceT IO Response
type LookupFN = (String -> ResourceT IO (Maybe String))
type InsertFN = (String -> String -> ResourceT IO ())
type BSAssoc = [(BS.ByteString, BS.ByteString)]
type SessionStore = Vault.Key (Session (ResourceT IO) String String)