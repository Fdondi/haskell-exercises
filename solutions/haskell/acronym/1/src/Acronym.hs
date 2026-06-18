module Acronym (abbreviate) where

import Data.Char (isUpper, isLower, toUpper, isAlpha)

import Data.Maybe (isJust, fromJust)

-- we want to take a letter when
-- it starts a word (follows a non-letter)
-- it is uppercase and follows a non-uppercase

data PreviousChar = Lowercase | Uppercase | NonLetter

toPreviousChar c
   | isUpper c = Uppercase
   | isLower c || c == '\'' = Lowercase
   | otherwise = NonLetter

takeLetter :: (PreviousChar, Char) -> Maybe Char
takeLetter (Uppercase, _)    = Nothing
takeLetter (NonLetter, char) = if (isAlpha char) then Just(toUpper char) else Nothing
takeLetter (Lowercase, char) = if isUpper char then Just(char) else Nothing

keepSuccess :: [Maybe a] -> [a]
keepSuccess maybes = map fromJust $ filter isJust maybes

abbreviate :: String -> String
abbreviate xs = toUpper (head xs) : keepSuccess processed
    where processed = map takeLetter $ zip (map toPreviousChar xs) (tail xs)
