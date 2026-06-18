module Bob (responseFor) where

import Data.Char 

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

isAllUpper [] hasUpper = hasUpper
isAllUpper (x:xs) hasUpper
     | isLower x = False
     | isUpper x = isAllUpper xs True
     | otherwise = isAllUpper xs hasUpper

responseFor :: String -> String
responseForImpl "" = "Fine. Be that way!"
responseForImpl xs = let isQuestion = (last xs == '?'); isYell = isAllUpper xs False 
                  in if isQuestion 
                      then if isYell 
                        then "Calm down, I know what I'm doing!"
                        else "Sure."
                      else if isYell 
                        then "Whoa, chill out!"
                        else "Whatever."

responseFor xs = responseForImpl $ trim xs