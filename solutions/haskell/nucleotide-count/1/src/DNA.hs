{-# LANGUAGE TupleSections #-}

module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, empty, fromListWith)
import Data.Foldable (foldl)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

-- Parse a Char into a Nucleotide, or return an error
nucleotideParse :: Char -> Either String Nucleotide
nucleotideParse 'A' = Right A
nucleotideParse 'C' = Right C
nucleotideParse 'G' = Right G
nucleotideParse 'T' = Right T
nucleotideParse c   = Left $ "Invalid nucleotide: " ++ [c]

-- Count all nucleotides in a String
nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts str = fromListWith (+) . map (, 1) <$> traverse nucleotideParse str
