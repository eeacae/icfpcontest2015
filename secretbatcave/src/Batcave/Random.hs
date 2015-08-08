module Batcave.Random
    ( randoms
    ) where

import Data.Word (Word32)

------------------------------------------------------------------------

-- | Generates an infinite list of random numbers given an initial seed.
--
--   Follows the C11 spec:
--     http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
--
randoms :: Word32 -> [Word32]
randoms = map mask . lcg
  where
    mask x = (x `div` 65536) `mod` 32768

lcg :: Word32 -> [Word32]
lcg seed = seed : lcg next
  where
    next = seed * multiplier + increment

    multiplier = 1103515245
    increment  = 12345
