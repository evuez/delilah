{-# LANGUAGE OverloadedStrings #-}

module Slack
  ( Message(..)
  , StatusMessage(..)
  , Response(..)
  , post
  ) where

import Data.Aeson (ToJSON(..), (.=), object)
import Lib (phrasecase)
import Network.HTTP.Simple
  ( getResponseStatusCode
  , httpLBS
  , parseRequest
  , setRequestBodyJSON
  )

data Message = Message
  { text :: String
  }

data StatusMessage = StatusMessage
  { service :: String
  , old :: String
  , new :: String
  , emoji :: String
  }

data Response
  = OK
  | Error String

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

instance Show StatusMessage where
  show (StatusMessage service' old' new' emoji') =
    concat
      [ emoji'
      , " Service update: `"
      , service'
      , "` changed its status from _"
      , phrasecase old'
      , "_ to *"
      , phrasecase new'
      , "*"
      ]

post :: String -> StatusMessage -> IO Response
post url statusMessage = do
  req' <- parseRequest $ concat ["POST ", url]
  let req = setRequestBodyJSON (Message $ show statusMessage) req'
  resp <- httpLBS req
  case getResponseStatusCode resp of
    200 -> return $ OK
    err -> return $ Error ("Request to Slack failed with HTTP " ++ show err)
