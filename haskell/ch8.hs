type Pos = (Int,Int)
type Trans = Pos -> Pos

type Pair a = (a,a)

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

data Move = North | South | East | West --constructors

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p 
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev South = North
rev North = South
rev East = West
rev West = East

--constructors have arguments
data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n 

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

--Maybe
