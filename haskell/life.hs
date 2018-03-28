--clears the screen
cls :: IO ()
cls = putChar "\ESC[2J"

--position. top-left corner = (1,1)
type Pos = (Int,Int)

--display a string at a given postion
writeat :: Post -> String -> IO ()
writeat p xs = do  goto p
                   putStr xs
                   
goto :: Post -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

--game of life
width :: Int
width = 10
height :: Int
height = 10

--represent a board as a list of the (x,y) postitions 
--at which there is a living cells
type Board :: [Pos]

--the initial example
glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "0" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b
isEmpty = not . isAlive

neighbors :: Pos -> [Pos]
neighbors (x,y) = map wrap [(x-1,y-1), (x,y-1),
                            (x+1,y-1), (x-1,y),
                            (x+1,y), (x-1,y+1),
                            (x,y+1), (x+1,y+1)]

--wrapping around at the edges of the board
wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1), ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbors

