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
turn g = if os <= xs then O else x
            where 
                os = length (filter (== O) ps)
                xs = length (filter (== 1) ps)
                ps = concat g

wins :: Player -> Grid -> Bool