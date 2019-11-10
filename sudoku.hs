{-|
Module      : Main
Description : Sudoku Solver in Haskell
Maintainer  : martijn.knegt@student.hu.nl
Stability   : experimental
Sample Input: .......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6...
-}
module Main where
import Data.List.Split
import Data.List
import Data.Char
import Data.Function
import Control.Applicative
import Control.Monad

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
    reducePossible (Possible xs) = case xs \\ fix of
      []  -> Nothing
      [y] -> Just $ Fixed y
      ys  -> Just $ Possible ys
    reducePossible x = Just x

-- |Turns grid into grid with 3x3 subgrids as rows
subGridsToRows :: Grid -> Grid
subGridsToRows = concatMap (\rows -> let [r1, r2, r3] = map (chunksOf 3) rows
                           in zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3) . chunksOf 3


-- |Reduce row, column, 3x3 subgrid possibilities
reduceGrid :: Grid -> Maybe Grid
reduceGrid grid =
  traverse reducePossibilities grid
  >>= fmap transpose . traverse reducePossibilities . transpose
  >>= fmap subGridsToRows . traverse reducePossibilities . subGridsToRows

-- |Reduce the grid until satisfaction
reduceGrid' :: Grid -> Maybe Grid
reduceGrid' = fixM reduceGrid
  where
    fixM f x = f x >>= \y -> if y == x then return x else fixM f y


-- |Checks if value is a 'Possible'
isPossible (Possible _) = True
isPossible _ = False

-- |Counts amount of possibilities
possibilityCount (Possible xs) = length xs
possibilityCount (Fixed _) = 1

-- |Fixes a cell for the nextGrids function: in case of 2 options return 2 Fixed values, else 1 Fixed and a list of the 'Possible' that remain
fixCell (i, Possible [x, y]) = (i, Fixed x, Fixed y)
fixCell (i, Possible (x:xs)) = (i, Fixed x, Possible xs)
fixCell _ = error "Impossible case"

-- |Replace value at specific position
replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0..]]

-- |Replace value in 2D (9x9 grid)
replace2D :: Int -> a -> [[a]] -> [[a]]
replace2D i v = let (x, y) = (i `quot` 9, i `mod` 9) in replace x (replace y (const v))

-- |Constructs 2 Grids for the next step, the modified cell is selected based on the least amount of 'Possible'
nextGrids :: Grid -> (Grid, Grid)
nextGrids grid = let (i, first@(Fixed _), rest) = fixCell . minimumBy (compare `Data.Function.on`(possibilityCount . snd)) . filter (isPossible . snd) . zip [0..] . concat $ grid
                 in (replace2D i first grid, replace2D i rest grid)

-- |Checks if the grid only contains Fixed values (is completed)
isComplete :: Grid -> Bool
isComplete grid = null [ () | Possible _ <- concat grid]

-- |Placeholder for recursive hasDuplicates' function
hasDuplicates d = hasDuplicates' d []

-- |Check recursively for duplicates in a list
hasDuplicates' [] _ = False
hasDuplicates' (y:ys) xs 
  | y `elem` xs = True 
  | otherwise = hasDuplicates' ys(y:xs)

-- |'isInvalidRow' checks if a row is invalid by looking for duplicate values
isInvalidRow row = let fixeds = [x | Fixed x <- row] 
                       emptyPossibles = [x | Possible x <- row, null x] in hasDuplicates fixeds || not (null emptyPossibles)

-- |'isInvalidRow' on rows, columns, 3x3 subgrids
isInvalid :: Grid -> Bool
isInvalid grid = any isInvalidRow grid 
  || any isInvalidRow (transpose grid) 
  || any isInvalidRow (subGridsToRows grid)

-- |Solves the sudoku by using the reduceGrid function and nextGrids method
solve :: Grid -> Maybe Grid
solve grid = reduceGrid grid >>= solve'
  where
    solve' g
      | isInvalid g = Nothing
      | isComplete g = Just g
      | otherwise = let (grid1, grid2) = nextGrids g 
                    in solve grid1 <|> solve grid2

-- |The 'showGrid' function prints the sudoku in a viewable 9x9 grid
showGrid :: Grid -> String
showGrid = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x) = show x
    showCell _ = "."

-- |The 'main' reads sudoku input and prints the solved sudoku
main :: IO()
main = do
  inputs <- lines <$> getContents
  forM_ inputs $ \input ->
    case makeGrid input of
      Nothing -> putStrLn "Invalid Input"
      Just grid -> case solve grid of
        Nothing -> putStrLn "No Solution"
        Just grid' -> putStrLn $ showGrid grid'