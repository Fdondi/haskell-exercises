module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

divisors :: Int -> [Int]
divisors n = [ d | d <- [1..(n `div` 2)], n `mod` d == 0]

classify :: Int -> Maybe Classification
classify n | n < 1 = Nothing
           | n == sumDivs = Just Perfect
           | n < sumDivs = Just Abundant
           | n > sumDivs = Just Deficient
  where sumDivs = sum $ divisors n