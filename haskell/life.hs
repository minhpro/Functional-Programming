import ListExtend

--clears the screen
cls :: IO ()
cls = putStr "\ESC[2J"

--position. top-left corner = (1,1)
type Pos = (Int,Int)

--display a string at a given postion
writeat :: Pos -> String -> IO ()
writeat p xs = do  goto p
                   putStr xs
                   
goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

--game of life
width :: Int
width = 100
height :: Int
height = 100

--represent a board as a list of the (x,y) postitions 
--at which there is a living cells
type Board = [Pos]

--the initial example
glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "0" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b
isEmpty b p = not (isAlive b p)

neighbors :: Pos -> [Pos]
neighbors (x,y) = map wrap [(x-1,y-1), (x,y-1),
                            (x+1,y-1), (x-1,y),
                            (x+1,y), (x-1,y+1),
                            (x,y+1), (x+1,y+1)]

--wrapping around at the edges of the board
wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbors

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board -> [Pos]
-- births b = [(x,y) | x <- [1..width], 
--                     y <- [1..height],
--                     isEmpty b (x,y),
--                     liveneighbs b (x,y) == 3]

births b = [p | p <- rmdups (concat (map neighbors b)),
                isEmpty b p,
                liveneighbs b p == 3]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do     
            cls
            showcells b 
            wait 500000
            life (nextgen b)  

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]