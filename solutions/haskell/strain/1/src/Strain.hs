module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard _ [] = []
discard p (x:xs) = if (p x) then filtered else x:filtered
                      where filtered = discard p xs

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep p (x:xs) = if (p x) then x:filtered else filtered
                      where filtered = keep p xs