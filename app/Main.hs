{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad ((<$!>), liftM)
import Data.Aeson (FromJSON(..), (.:), withObject)
import Data.Char (toUpper)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T (concat, pack)
import Network.HTTP.Types.Status (status403)
import System.Environment (getEnvironment, lookupEnv)
import Web.Scotty (ActionM, finish, jsonData, param, post, scotty, text)
import qualified Web.Scotty as S (status)

data Config = Config
  { port :: Int
  , tokens :: [(String, String)]
  }

data Status = Status
  { meta :: StatusMeta
  , page :: StatusPage
  , component :: StatusComponent
  , update :: StatusUpdate
  }

data StatusMeta = StatusMeta
  { unsubscribe :: String
  , documentation :: String
  }

data StatusPage = StatusPage
  { id :: String
  , statusIndicator :: String
  , statusDescription :: String
  }

data StatusComponent = StatusComponent
  { name :: String
  , status :: String
  }

data StatusUpdate = StatusUpdate
  { old :: String
  , new :: String
  }

instance FromJSON Status where
  parseJSON =
    withObject "Status" $ \v ->
      Status <$> v .: "meta" <*> v .: "page" <*> v .: "component" <*>
      v .: "component_update"

instance FromJSON StatusMeta where
  parseJSON =
    withObject "StatusMeta" $ \v ->
      StatusMeta <$> v .: "unsubscribe" <*> v .: "documentation"

instance FromJSON StatusPage where
  parseJSON =
    withObject "StatusPage" $ \v ->
      StatusPage <$> v .: "id" <*> v .: "status_indicator" <*>
      v .: "status_description"

instance FromJSON StatusComponent where
  parseJSON =
    withObject "StatusComponent" $ \v ->
      StatusComponent <$> v .: "name" <*> v .: "status"

instance FromJSON StatusUpdate where
  parseJSON =
    withObject "StatusUpdate" $ \v ->
      StatusUpdate <$> v .: "old_status" <*> v .: "new_status"

getPort :: IO Int
getPort = read <$!> liftM (fromMaybe "3000") (lookupEnv "PORT")

getTokens :: IO [(String, String)]
getTokens = filter (isPrefixOf "SERVICE_TOKEN_" . fst) <$!> getEnvironment

getConfig :: IO Config
getConfig = do
  port' <- getPort
  tokens' <- getTokens
  return Config {port = port', tokens = tokens'}

main :: IO ()
main = getConfig >>= run

run :: Config -> IO ()
run c = do
  port' <- getPort
  scotty port' $ post "/status/:service" (auth c >> checkStatus) -- >> S.status status204 >> empty

auth :: Config -> ActionM ()
auth c = do
  service <- param "service" :: ActionM String
  token <- param "token" :: ActionM String
  case serviceToken c service of
    Just envToken
      | token == envToken -> text "OK"
    _ -> (S.status status403 >> finish)

serviceToken :: Config -> String -> Maybe String
serviceToken Config {tokens = tokens'} service = snd <$> find token tokens'
  where
    token = \(x, _) -> x == ("SERVICE_TOKEN_" ++ key)
    key =
      [ if x == '-'
        then '_'
        else toUpper x
      | x <- service
      ]

checkStatus :: ActionM ()
checkStatus = liftM update (jsonData :: ActionM Status) >>= handleUpdate

handleUpdate :: StatusUpdate -> ActionM ()
handleUpdate (StatusUpdate "operational" new') =
  text $ T.concat ["Outage! New status: ", T.pack new']
handleUpdate (StatusUpdate old' "operational") =
  text $ T.concat ["Back to normal! Old status: ", T.pack old']
handleUpdate (StatusUpdate old' new') =
  text $ T.concat [T.pack old', " -> ", T.pack new']
