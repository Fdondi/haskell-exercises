module DNA (toRNA) where

import Data.Traversable (traverse)

toRNAOne :: Char -> Either Char Char
toRNAOne 'G' = Right 'C'
toRNAOne 'C' = Right 'G'
toRNAOne 'T' = Right 'A'
toRNAOne 'A' = Right 'U'
toRNAOne c   = Left c

toRNA :: String -> Either Char String
toRNA = traverse toRNAOne
                  
