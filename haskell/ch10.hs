import Data.Char
import System.IO

--from nim.hs
newline :: IO ()
newline = putChar '\n'

getNat :: String -> IO Int
getNat prompt = do  
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs then
      return (read xs)
  else 
    do  putStrLn "ERROR: Invalid number"
        getNat prompt

strToInt :: String -> Int
strToInt = foldl (\x y -> x * 10 + digitToInt y) 0

--Exercise 1
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar c | c <- xs]

--Exercise 2 done in nim.hs
--Exercise 3 done in nim.hs

--Exercise 4
--from nim.hs
getDigit :: String -> IO Int
getDigit prompt = do  
  putStr prompt
  x <- getChar
  newline
  if isDigit x then
    return (digitToInt x)
  else 
    do  putStrLn "ERROR: Invalid digit"
        getDigit prompt

adder :: IO ()
adder = do
  n <- getDigit "How many numbers? "
  summer n []

summer :: Int -> [Int] -> IO ()
summer n xs | n == 0 = putStrLn ("The total is " ++ (show (sum xs)))
            | otherwise = do
                x <- getNat []
                summer (n-1) (x:xs)

--Exercise 5
adder' :: IO ()
adder' = do 
  n <- getDigit "How many numbers? "
  xs <- sequence [getNat [] | _ <- [1..n]]
  putStrLn ("The total is " ++ (show (sum xs)))

--Exercise 6
--readLine backspace feature
readLine :: IO String
readLine = readLine' []

readLine' :: String -> IO String
readLine' xs = do  
  x <- getCh
  if x == '\DEL' then do  
    putChar '\b'
    readLine' (removeLast xs)
  else do  
    putChar x
    if x == '\n' then
      return xs
    else do
      readLine' (xs ++ [x])

removeLast :: String -> String
removeLast [] = []
removeLast [x] = []
removeLast (x:xs) = x : removeLast xs

--from hangman.hs
getCh :: IO Char
getCh = do  hSetEcho stdin False
            x <- getChar
            hSetEcho stdin True
            return x

--clears the screen
cls :: IO ()
cls = putStr "\ESC[2J"