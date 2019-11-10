{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad ((<$!>), liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, lift, runReaderT)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T (Text, pack)
import Lib (snakecase)
import Network.HTTP.Types.Status (status204, status403, status502)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setPort)
import qualified Slack as Sl (Response(..), StatusMessage(..), post)
import qualified StatusPage as SP (Status(..), StatusUpdate(..))
import System.Environment (getEnv, getEnvironment, lookupEnv)
import Web.Scotty (Options(..))
import Web.Scotty.Trans
  ( ActionT
  , finish
  , jsonData
  , param
  , post
  , scottyOptsT
  , text
  )
import qualified Web.Scotty.Trans as S (status)

data Config = Config
  { tokens :: [(String, String)]
  , slackURL :: String
  }

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type ActionD a = ActionT T.Text ConfigM a

tokenPrefix :: String
tokenPrefix = "SERVICE_TOKEN_"

getPort :: IO Int
getPort = read <$!> liftM (fromMaybe "3000") (lookupEnv "PORT")

getTokens :: IO [(String, String)]
getTokens = filter (isPrefixOf tokenPrefix . fst) <$!> getEnvironment

getConfig :: IO Config
getConfig = do
  tokens' <- getTokens
  slackURL' <- getEnv "SLACK_URL"
  return Config {tokens = tokens', slackURL = slackURL'}

getSettings :: IO Settings
getSettings = liftM (flip setPort defaultSettings) getPort

getOptions :: IO Options
getOptions = liftM (Options 1) getSettings

main :: IO ()
main = do
  o <- getOptions
  c <- getConfig
  let r m = runReaderT (runConfigM m) c
  scottyOptsT o r $ post "/status/:service" (auth >> checkStatus)

auth :: ActionD ()
auth = do
  service <- param "service"
  token <- param "token"
  tokens' <- lift $ asks tokens
  if (tokenPrefix ++ snakecase service, token) `elem` tokens'
    then text "OK"
    else S.status status403 >> finish

checkStatus :: ActionD ()
checkStatus = do
  url <- lift $ asks slackURL
  service <- param "service"
  res <- liftM SP.update jsonData >>= handleUpdate url service
  case res of
    Sl.OK -> S.status status204
    Sl.Error err -> text (T.pack err) >> S.status status502

handleUpdate :: String -> String -> SP.StatusUpdate -> ActionD Sl.Response
handleUpdate url service (SP.StatusUpdate old'@"operational" new') =
  liftIO $ Sl.post url $ Sl.StatusMessage service old' new' ":fire:"
handleUpdate url service (SP.StatusUpdate old' new'@"operational") =
  liftIO $ Sl.post url $ Sl.StatusMessage service old' new' ":heavy_check_mark:"
handleUpdate url service (SP.StatusUpdate old' new') =
  liftIO $ Sl.post url $ Sl.StatusMessage service old' new' ":radio:"
