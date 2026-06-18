module Triangle (rows) where

sumTuple (a,b) = a+b

continue :: Int -> [Integer] -> [[Integer]]
continue 0 _ = []
continue n prevRow = newRow : (continue (n-1) newRow)
    where
      newRow = map sumTuple $ zip ([0] ++ prevRow) (prevRow ++ [0])

rows :: Int -> [[Integer]]
rows 0 = []
rows n = [1]:continue (n-1) [1]
