module Phone (number) where

import Data.Char(isDigit, digitToInt)

hasValidPrefix :: [Char] -> Bool
hasValidPrefix (x:_) = (digitToInt x) > 1

isValid cleanedNumber = length subscriberNumber == 4 && hasValidPrefix areaCode && hasValidPrefix exchangeCode
  where 
    (exchangeCode, subscriberNumber) = splitAt 3 localNumber
    (areaCode, localNumber) = splitAt 3 cleanedNumber
    
number :: String -> Maybe String
number xs = if isValid cleanedNumber then Just cleanedNumber else Nothing
  where
    cleanedNumber = removeOne $ removePunctuation xs
    removePunctuation = filter isDigit
    removeOne (d:ds) = if d == '1' then ds else (d:ds)
    
