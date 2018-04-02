import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

--O, B, or X. B represents a blank space
data Player = O | B | X
                deriving (Eq, Ord, Show)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next X = O

--Grid utilities
empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
            where 
                os = length (filter (== O) ps)
                xs = length (filter (== X) ps)
                ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
                where
                    line = all (==p)
                    rows = g
                    cols = transpose g
                    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

--Displaying a grid
--   |   |   
-- O | X | O
--   |   |
putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
            where
                bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
            where
                beside  = foldl1 (zipWith (++))
                bar     = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys 

--Making a move
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B
 
--if valid return singleton list else return empty list
move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
                where 
                    (xs, B:ys) = splitAt i (concat g)

--break a list to maximal segments of a given length
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

--get move index from user
getNat :: String -> IO Int
getNat prompt = do  
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs then
      return (read xs)
  else 
    do  putStrLn "ERROR: Invalid number"
        getNat prompt

--Human vs human
tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do  
    cls
    goto (1,1)
    putGrid g
    run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g   = putStrLn "It's a draw!\n"
         | otherwise = 
            do  i <- getNat (prompt p)
                case move g i p of
                    []  -> do  putStrLn "ERROR: Invalid move"
                               run' g p
                    [g'] -> do run g' (next p)  
                    
prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move "

--clears the screen
cls :: IO ()
cls = putStr "\ESC[2J"

--position. top-left corner = (1,1)
type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


 