module SumOfMultiples (sumOfMultiples) where

energyPoints :: Integer -> Integer -> [Integer]
energyPoints level 0    = []
energyPoints level item = takeWhile (<level) $ map(*item) [1..]

-- Merge two sorted lists, removing duplicates
mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted [] ys = ys
mergeSorted xs [] = xs
mergeSorted (x:xs) (y:ys)
  | x < y     = x : mergeSorted xs (y:ys)
  | x > y     = y : mergeSorted (x:xs) ys
  | otherwise = x : mergeSorted xs ys

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ foldl mergeSorted [] $ map (energyPoints limit) factors
