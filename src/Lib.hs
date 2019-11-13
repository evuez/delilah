module Lib
  ( snakecase
  , phrasecase
  ) where

import Data.Char (toUpper)

snakecase :: String -> String
snakecase s =
  [ if x == '-'
    then '_'
    else toUpper x
  | x <- s
  ]

phrasecase :: String -> String
phrasecase s =
  [ if x `elem` "-_"
    then ' '
    else x
  | x <- s
  ]
