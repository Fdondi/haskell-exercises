module Anagram (anagramsFor) where

import Data.List(sort)
import Data.Char(toLower)

toLowerString :: String -> String
toLowerString [] = []
toLowerString (x:xs) = toLower x : toLowerString xs

(=~=) :: String -> String -> Bool
(=~=) a b = sort a == sort b

isAnagram :: String -> String -> Bool
isAnagram target word = l_target /= l_word && l_target =~= l_word where
                      l_target = toLowerString target
                      l_word = toLowerString word

anagramsFor :: String -> [String] -> [String]
anagramsFor target words = filter (isAnagram target) words
