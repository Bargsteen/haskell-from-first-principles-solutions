module IPV4 where

import Data.Word
import Data.Bits


data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

