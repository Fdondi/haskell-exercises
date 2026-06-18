module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)
import Data.Bits (setBit, zeroBits, shiftL, (.&.))

-- Target bitmask: all 26 bits set (for a-z)
target :: Int
target = (1 `shiftL` 26) - 1

-- Recursive helper function
go :: String -> Int -> Bool
go _ mask | mask == target = True  -- Early exit if target is reached
go [] _ = False                    -- No more characters, but target not reached
go (c:cs) mask = go cs (if isAlpha c
                then setBit mask (fromEnum (toLower c) - fromEnum 'a')
                else mask)

-- Main function
isPangram :: String -> Bool
isPangram sentence = go sentence 0