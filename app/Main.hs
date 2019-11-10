{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad ((<$!>), liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
import Web.Scotty (verbose, settings, Options(..))
import Web.Scotty.Trans (scottyOptsT, ActionT, param, post, text, jsonData, finish)
import qualified Web.Scotty.Trans as S (status)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setPort)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT, lift)
import qualified Data.Text.Lazy as T (Text, pack)

data Config = Config
  {
   tokens :: [(String, String)]
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
  { statusIndicator :: String
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
      StatusPage <$> v .: "status_indicator" <*> v .: "status_description"

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
  tokens' <- getTokens
  slackURL' <- getEnv "SLACK_URL"
  return Config {tokens = tokens', slackURL = slackURL'}

getOptions :: IO Options
getOptions = do
  s <- getSettings
  return Options { settings = s , verbose = 1 }

getSettings :: IO Settings
getSettings = liftM (flip setPort defaultSettings) getPort

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)


type ActionD a = ActionT T.Text ConfigM a

main :: IO ()
main = do
  o <- getOptions
  c <- getConfig
  let r m = runReaderT (runConfigM m) c
  scottyOptsT o r $ post "/status/:service" (auth >> checkStatus >> S.status status204)


getHome :: ActionD ()
getHome = do
  slackURL' <- lift $ asks slackURL
  service <- param "service"
  text $ mconcat [service, T.pack slackURL']


auth :: ActionD ()
auth = do
  service <- param "service" :: ActionD String
  token <- param "token" :: ActionD String
  tokens' <- lift $ asks tokens
  case serviceToken tokens' service of
    Just t
      | token == t -> text "OK"
    _ -> (S.status status403 >> finish)

serviceToken :: [(String, String)] -> String -> Maybe String
serviceToken tokens' service = snd <$> find token tokens'
  where
    token = \(x, _) -> x == ("SERVICE_TOKEN_" ++ key)
    key =
      [ if x == '-'
        then '_'
        else toUpper x
      | x <- service
      ]

checkStatus :: ActionD ()
checkStatus = do
  url <- liftIO $ getEnv "SLACK_URL"
  service <- param "service"
  liftM update (jsonData :: ActionD Status) >>= handleUpdate url service

handleUpdate :: String -> String -> StatusUpdate -> ActionD ()
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
