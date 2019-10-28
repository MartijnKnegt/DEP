{-|
Module      : Sudoku
Description : Sudoku Solver in Haskell
Maintainer  : martijn.knegt@student.hu.nl
Stability   : experimental
-}
module Sudoku where
import Data.List.Split
import Data.List
import Data.Char

data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)
type Row = [Cell]
type Grid = [Row]



-- |The 'getCellData' function checks for a Fixed number, if not, a Possible 1-9 list is assigned
getCellData :: Char -> Maybe Cell
getCellData '.' = Just $ Possible $ [1..9]
getCellData c | isDigit c && c > '0' = Just $ Fixed $ digitToInt(c) | otherwise = Nothing

-- |The 'makeGrid' function creates a grid by running getCellData over every char in a string
makeGrid :: String -> Maybe Grid
makeGrid x | length x == 81 = traverse (traverse getCellData) . chunksOf 9 $ x | otherwise = Nothing

-- |Checks for Fixed values in a row, takes the Fixed number out of every possibility in that row
reducePossibilities :: [Cell] -> Maybe [Cell]
reducePossibilities cells = traverse reducePossible cells
  where
    fix = [x | Fixed x <- cells]
    reducePossible (Possible xs) = case xs Data.List.\\ fix of
      []  -> Nothing
      [y] -> Just $ Fixed y
      ys  -> Just $ Possible ys
    reducePossible x = Just x

s = ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."
Just grid = makeGrid s
Just grid' = traverse reducePossibilities grid --reduce row possibles
Just grid'' = fmap transpose . traverse reducePossibilities . transpose $ grid' --reduce column possibles

-- |The 'main' prints the grid with reduced possibilities in Rows and Columns
main :: IO()
main = putStrLn $ show (grid'')
