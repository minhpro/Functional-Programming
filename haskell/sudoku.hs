import ListExtend
import Data.List

divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n xs = take n xs : divide n (drop n xs)

sudoku :: [[Char]]
sudoku = [['.', '9', '.', '.', '2', '7', '.', '.', '6'],
            ['2', '.', '5', '.', '.', '1', '.', '.', '.'],
            ['.', '.', '.', '.', '9', '.', '.', '5', '8'],
            ['.', '.', '1', '4', '.', '.', '.', '8', '.'],
            ['.', '.', '.', '.', '5', '8', '1', '6', '.'],
            ['5', '2', '8', '.', '.', '.', '.', '.', '.'],
            ['4', '3', '.', '.', '8', '.', '6', '.', '.'],
            ['7', '.', '2', '6', '.', '.', '.', '4', '.'],
            ['.', '.', '.', '7', '.', '2', '3', '.', '2']]

squareDivide :: Int -> [[a]] -> [[a]]
squareDivide n xs = concat (map (foldn (++) []) divided)
                        where divided = divide n (map (divide n) xs)

size :: Int
size = 3

hidden :: Char
hidden = '.'

isValidSudoku :: [[Char]] -> Bool
isValidSudoku grid = all valid (grid ++ transpose grid ++ squareDivide size grid)

valid :: [Char] -> Bool
valid [] = True
valid [_] = True
valid (x:xs) | x == '.' = valid xs
             | elem x xs = False
             | otherwise = valid xs

--From LudvikGalois
sudoku2 grid = all valid rows && all valid cols && all valid boxes
  where
    rows = grid
    cols = transpose grid
    boxes = map concat $ concatMap transpose $ splitInto3 $ map splitInto3 grid
    splitInto3 (x:y:z:xs) = [x,y,z] : splitInto3 xs
    splitInto3 [] = []
    valid block = let b = filter (/= '.') block in b == (nub b)