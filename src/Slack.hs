{-# LANGUAGE OverloadedStrings #-}

module Slack
  ( Message(..)
  ) where

import Data.Aeson (ToJSON(..), (.=), object)

data Message = Message
  { text :: String
  }

instance ToJSON Message where
  toJSON (Message text') =
    object
      [ "text" .= text'
      , "blocks" .=
        [ object
            [ "type" .= ("section" :: String)
            , "text" .= object ["type" .= ("mrkdwn" :: String), "text" .= text']
            ]
        ]
      ]
