{-# LANGUAGE OverloadedStrings #-}

module StatusPage
  ( Status(..)
  , Update(..)
  , Component(..)
  ) where

import Data.Aeson (FromJSON(..), (.:), withObject)

data Status = Status
  { update :: Update
  , component :: Component
  }

data Update = Update
  { old :: String
  , new :: String
  }

data Component = Component
  { name :: String
  }

instance FromJSON Status where
  parseJSON =
    withObject "Status" $ \v ->
      Status <$> v .: "component_update" <*> v .: "component"

instance FromJSON Update where
  parseJSON =
    withObject "Update" $ \v ->
      Update <$> v .: "old_status" <*> v .: "new_status"

instance FromJSON Component where
  parseJSON = withObject "Component" $ \v -> Component <$> v .: "name"
