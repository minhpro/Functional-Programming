--Let's combine functions with I/O to create a program that will:
--1. Ask the user to insert a string
--2. Read their string
--3. Capitalize all the letters from the string
--4. Write the resulting string

import Data.Char (toUpper)
import Control.Monad

main = putStrLn "Write your string: " >> fmap shout getLine >>= putStrLn

shout = map toUpper

main' = putStrLn "Write your string: " >> getLine >>= (putStrLn . shout) 

main'' = do
    putStrLn "Write your string: "
    str <- getLine
    putStrLn (shout str)

-- Monadic control structures

-- fiveGetLines = replicate 5 getLine
-- to get five lines of user input but it doesn't work

-- sequence :: Monad m => [m a] -> m [a] 
fiveGetLines = sequence $ replicate 5 getLine
-- OR -- combination of sequence and replicate -> replicateM
fiveGetLines' = replicateM 5 getLine

sequenceAlt :: Monad m => [m a] -> m [a]
sequenceAlt [] = return []
sequenceAlt (m:ms) = do
    x <- m
    xs <- sequenceAlt ms 
    return (x:xs)

-- other idea using foldr


fiveGetLinesAlt = sequenceAlt $ replicate 5 getLine

-- combination of sequence and map
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
