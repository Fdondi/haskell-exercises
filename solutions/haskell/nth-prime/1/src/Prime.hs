module Prime (nth) where

import Data.List(find)
hasDivisor n [] = False
hasDivisor n (d:ds) | d*d > n = False
                    | n `mod` d == 0 = True
                    | otherwise = hasDivisor n ds

filterPrimes :: [Integer] -> Integer -> [Integer] 
filterPrimes knownPrimes candidate = if not $ hasDivisor candidate knownPrimes 
                                     then candidate:filterPrimes (knownPrimes ++ [candidate]) (candidate+1)
                                     else filterPrimes knownPrimes (candidate+1)

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just $ (filterPrimes [] 2)!!(n-1)
