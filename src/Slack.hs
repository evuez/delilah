{-# LANGUAGE OverloadedStrings #-}

module Slack
  ( Message(..)
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
  { service :: String
  , component :: String
  , old :: String
  , new :: String
  , emoji :: String
  }

data Response
  = OK
  | Error String

instance ToJSON Message where
  toJSON message =
    object
      [ "text" .= section message
      , "blocks" .=
        [ object
            [ "type" .= ("section" :: String)
            , "text" .=
              object ["type" .= ("mrkdwn" :: String), "text" .= section message]
            ]
        , object
            [ "type" .= ("context" :: String)
            , "elements" .=
              [ object
                  ["type" .= ("mrkdwn" :: String), "text" .= context message]
              ]
            ]
        ]
      ]

post :: String -> Message -> IO Response
post url message = do
  req' <- parseRequest $ concat ["POST ", url]
  let req = setRequestBodyJSON message req'
  resp <- httpLBS req
  case getResponseStatusCode resp of
    200 -> return $ OK
    err -> return $ Error ("Request to Slack failed with HTTP " ++ show err)

section :: Message -> String
section (Message service' _ old' new' emoji') =
  concat
    [ emoji'
    , " *"
    , service'
    , "* changed its status from `"
    , phrasecase old'
    , "` to `"
    , phrasecase new'
    , "`"
    ]

context :: Message -> String
context (Message {component = component'}) = "*component*: " ++ component'
