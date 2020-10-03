{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, lift, runReaderT)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T (Text, pack)
import Lib (snakecase)
import Network.HTTP.Types.Status (status202, status204, status403, status502)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setPort)
import qualified Slack as Sl (Message(..), Response(..), post)
import qualified StatusPage as SP (Component(..), Status(..), Update(..))
import System.Environment (getEnv, getEnvironment, lookupEnv)
import Web.Scotty (Options(..))
import Web.Scotty.Trans
  ( ActionT
  , defaultHandler
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
getPort = read <$> fmap (fromMaybe "3000") (lookupEnv "PORT")

getTokens :: IO [(String, String)]
getTokens = filter (isPrefixOf tokenPrefix . fst) <$> getEnvironment

getConfig :: IO Config
getConfig = do
  tokens' <- getTokens
  slackURL' <- getEnv "SLACK_URL"
  return Config {tokens = tokens', slackURL = slackURL'}

getSettings :: IO Settings
getSettings = (`setPort` defaultSettings) <$> getPort

getOptions :: IO Options
getOptions = Options 1 <$> getSettings

main :: IO ()
main = do
  o <- getOptions
  c <- getConfig
  let r m = runReaderT (runConfigM m) c
  scottyOptsT o r $
    defaultHandler (const $ S.status status202) >>
    post "/status/:service" (auth >> checkStatus)

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
  res <- liftIO . Sl.post url . message service =<< jsonData
  case res of
    Sl.OK -> S.status status204
    Sl.Error err -> text (T.pack err) >> S.status status502

message :: String -> SP.Status -> Sl.Message
message service (SP.Status (SP.Update old new) (SP.Component component)) =
  let emoji =
        case (old, new) of
          ("operational", _) -> ":fire:"
          (_, "operational") -> ":heavy_check_mark:"
          (_, _) -> ":radio:"
  in Sl.Message service component old new emoji
