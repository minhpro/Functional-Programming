import Data.Char

--from nim.hs
newline :: IO ()
newline = putChar '\n'

getDigit :: String -> IO Int
getDigit prompt = do  putStr prompt
                      x <- getChar
                      newline
                      if isDigit x then
                        return (digitToInt x)
                      else 
                        do  putStrLn "ERROR: Invalid digit"
                            getDigit prompt

--Exercise 1
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar c | c <- xs]

--Exercise 2 done in nim.hs
--Exercise 3 done in nim.hs

--Exercise 4
adder :: IO ()
adder = do
            n <- getDigit "How many numbers? "
            summer n []

summer :: Int -> [Int] -> IO ()
summer n xs | n == 0 = putStrLn ("The total is " ++ (show (sum xs)))
            | otherwise = do
                            x <- getDigit []
                            summer (n-1) (x:xs)
