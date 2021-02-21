-- |
module Utils where

import Data.Word (Word16)
import Text.Printf (printf)

class PP a where
  pp :: a -> String

instance PP Word16 where
  pp = printf "%04x"
