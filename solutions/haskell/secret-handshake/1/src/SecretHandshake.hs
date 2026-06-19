module SecretHandshake (handshake) where

moves :: [String]
moves = ["wink", "double blink", "close your eyes", "jump"] 

selectMoves :: Int -> [String] -> [String] 
selectMoves 0 _      = []
selectMoves n (m:ms) = if odd n then m:selectMoves (n `div` 2) ms else selectMoves (n `div` 2) ms

handshake :: Int -> [String]
handshake n = if n >= 16 then reverse (selectMoves (n `mod` 16) moves) else (selectMoves n moves)
