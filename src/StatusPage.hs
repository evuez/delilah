{-# LANGUAGE OverloadedStrings #-}

module StatusPage
  ( Status(..)
  , StatusUpdate(..)
  ) where

import Data.Aeson (FromJSON(..), (.:), withObject)

data Status = Status
  { update :: StatusUpdate
  }

data StatusUpdate = StatusUpdate
  { old :: String
  , new :: String
  }

instance FromJSON Status where
  parseJSON = withObject "Status" $ \v -> Status <$> v .: "component_update"

instance FromJSON StatusUpdate where
  parseJSON =
    withObject "StatusUpdate" $ \v ->
      StatusUpdate <$> v .: "old_status" <*> v .: "new_status"
