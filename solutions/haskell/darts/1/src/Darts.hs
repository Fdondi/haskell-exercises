module Darts (score) where

distance :: Float -> Float -> Float
distance x y = sqrt (x*x+y*y)

scoreImpl :: Float -> Int
scoreImpl distance | distance <= 1 = 10
                   | distance <= 5 = 5
                   | distance <= 10 = 1
                   | otherwise = 0

score :: Float -> Float -> Int
score x y = scoreImpl (distance x y)
              
