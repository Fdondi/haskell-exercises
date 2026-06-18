module Grains (square, total) where

import Data.Maybe (fromJust)

square :: Integer -> Maybe Integer
square n = if (n > 0 && n <= 64) then Just(2^(n-1)) else Nothing

data Square = Square Integer

makeSquare :: Integer -> Maybe Square 
makeSquare n = if ((n > 0) && (n <= 64)) then Just(Square n) else Nothing

squareSafe :: Square -> Integer
squareSafe (Square n) = 2^(n-1)


total :: Integer
total = sum $ map (fromJust . square) [1..64]
