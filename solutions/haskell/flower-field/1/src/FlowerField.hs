module FlowerField (annotate) where

import Data.Char(intToDigit)
import Data.List(zip7)

countFlowers :: ((Char, Char, Char), (Char, Char, Char), (Char, Char, Char)) -> Char
countFlowers ((_, '*', _), _, _)                                 = '*'
countFlowers ((' ', ' ', ' '), (' ', ' ', ' '), (' ', ' ', ' ')) = ' '
countFlowers ((cr1, ' ', cr3), (pr1, pr2, pr3), (nr1,nr2, nr3))                             = intToDigit(numFlowers)
    where numFlowers = length $ filter (=='*') [pr1, pr2, pr3, cr1, cr3, nr1, nr2, nr3]

explode:: String -> [(Char,Char,Char)]
explode xs = zip3 (" " ++ xs) xs ((tail xs) ++ " ") 

countFlowersRow :: [(Char, Char, Char)] -> [(Char, Char, Char)] -> [(Char, Char, Char)] -> String
countFlowersRow explodedPrevRow explodedCurrRow explodedNextRow = map countFlowers $ zip3 explodedCurrRow explodedPrevRow explodedNextRow

annotateImpl :: [(Char, Char, Char)] -> [(Char, Char, Char)] -> [String] -> [String]
annotateImpl explodedPrevRow explodedCurrRow [] = [countFlowersRow explodedPrevRow explodedCurrRow (repeat (' ',' ',' '))]
annotateImpl explodedPrevRow explodedCurrRow (nextRow:restBoard) = (countFlowersRow explodedPrevRow explodedCurrRow explodedNextRow) : (annotateImpl explodedCurrRow explodedNextRow restBoard) where explodedNextRow = explode nextRow

annotate :: [String] -> [String]
annotate [] = []
annotate (firstRow:restBoard) = annotateImpl (repeat (' ',' ',' ')) (explode firstRow) restBoard
