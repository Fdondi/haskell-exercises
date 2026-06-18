module LeapYear (isLeapYear) where

divisor year = if year `mod` 100 == 0 then 400 else 4

isLeapYear :: Integer -> Bool
isLeapYear year = year `mod` (divisor year) == 0
