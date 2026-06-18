module Clock (addDelta, fromHourMin, toString) where

import Text.Printf

newtype Clock = Clock Int
  deriving Eq

fromMins :: Int -> Clock
fromMins mins = Clock (mins `mod` 1440)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = fromMins (hour * 60 + min)

toString :: Clock -> String
toString (Clock total_minutes) = printf "%02d:%02d" hours minutes
  where
    hours = total_minutes `div` 60
    minutes = total_minutes `mod` 60
    
addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock old_mins) = fromMins (old_mins + min + (hour * 60))
