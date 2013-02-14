{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Default (def)
import Data.String (fromString)
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.Vault as Vault

import Data.Acid (AcidState, Update, Query, makeAcidic, openLocalState)
import Data.Acid.Advanced (query')
import Data.Acid.Local (createCheckpointAndClose)
import Data.IxSet ((@=), Proxy(..), getOne)

import Network.Wai
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Network.Wai.Session (withSession)
import Network.Wai.Session.Map (mapStore_)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (ok200, unauthorized401, status404)

import Control.Exception (bracket)
import Control.Concurrent.Chan (Chan, newChan, dupChan, writeChan)
import Control.Monad.Trans.Resource (ResourceT)
    
import TypeSynonyms
import Util
import Model
import Handlers

routes :: DB -> SessionStore -> Request -> RES
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
      ["comment"] ->
        withParams params ["comment"] $ commentItem db user item . head
      ["count"] -> 
        withParams params ["count"] changeCount
        where changeCount [ct] = case reads ct :: [(Integer, String)] of
                (count, _):_ -> countItem db user item count
                _ -> resError "Count needs to be readable as a number"
      _ -> res404
    Nothing -> resError "Invalid item"

----- Server start
main = do
  session <- Vault.newKey
  store <- mapStore_
  bracket (openLocalState initialDB) (createCheckpointAndClose) 
    (\db -> run 3000 . withSession store (fromString "SESSION") def session $ routes db session)