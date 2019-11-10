{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad ((<$!>), liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), (.:), withObject)
import Data.Char (toUpper)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromMaybe)
import Network.HTTP.Simple
  ( getResponseStatusCode
  , httpLBS
  , parseRequest
  , setRequestBodyJSON
  )
import Network.HTTP.Types.Status (status204, status403)
import qualified Slack as Sl (Message(..))
import System.Environment (getEnv, getEnvironment, lookupEnv)
import Web.Scotty (ActionM, finish, jsonData, param, post, scotty, text)
import qualified Web.Scotty as S (status)

data Config = Config
  { port :: Int
  , tokens :: [(String, String)]
  , slackURL :: String
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
  slackURL' <- getEnv "SLACK_URL"
  return Config {port = port', tokens = tokens', slackURL = slackURL'}

main :: IO ()
main = getConfig >>= run

run :: Config -> IO ()
run c = do
  port' <- getPort
  scotty port' $
    post "/status/:service" (auth c >> checkStatus >> S.status status204)

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
checkStatus = do
  url <- liftIO $ getEnv "SLACK_URL"
  service <- param "service"
  liftM update (jsonData :: ActionM Status) >>= handleUpdate url service

handleUpdate :: String -> String -> StatusUpdate -> ActionM ()
handleUpdate url service (StatusUpdate "operational" new') =
  liftIO $ notify url service "operational" new' ":fire:"
handleUpdate url service (StatusUpdate old' "operational") =
  liftIO $ notify url service old' "operational" ":heavy_check_mark:"
handleUpdate url service (StatusUpdate old' new') =
  liftIO $ notify url service old' new' ":radio:"

notify :: String -> String -> String -> String -> String -> IO ()
notify url service oldStatus newStatus emoji = do
  req' <- parseRequest $ concat ["POST ", url]
  let req =
        setRequestBodyJSON (notifyBody service oldStatus newStatus emoji) req'
  resp <- httpLBS req
  putStrLn $ "Got HTTP " ++ show (getResponseStatusCode resp) -- TODO: 500 if status code != 200

notifyBody :: String -> String -> String -> String -> Sl.Message
notifyBody service oldStatus newStatus emoji =
  Sl.Message $
  concat
    [ emoji
    , " Service update: `"
    , service
    , "` changed its status from _"
    , oldStatus
    , "_ to *"
    , newStatus
    , "*"
    ]
