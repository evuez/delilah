module Lib
  ( snakecase
  ) where

import Data.Char (toUpper)

snakecase :: String -> String
snakecase s =
  [ if x == '-'
    then '_'
    else toUpper x
  | x <- s
  ]
