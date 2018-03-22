type Pos = (Int,Int)
type Trans = Pos -> Pos

type Pair a = (a,a)

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

data Move = North | South | East | West --constructors
                deriving (Eq, Ord, Show, Read)

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
                deriving (Eq, Ord, Show, Read)

square :: Float -> Shape
square n = Rect n n 

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

--Maybe
catMaybe :: Maybe a -> a 
catMaybe (Just x) = x

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

--New type 
newtype Nongative = N Int 
            deriving (Eq, Ord, Show, Read)

--Recursive type
data Nat = Zero | Succ Nat 
            deriving (Eq, Ord, Show, Read)

nat2int :: Nat -> Int 
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat 
int2nat 0 = Zero 
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n 
add (Succ m) n = Succ (add m n)

--our version of List type
data List a = Nil | Cons a (List a)
                deriving (Eq, Ord, Show, Read)

len :: List a -> Int 
len Nil = 0
len (Cons _ xs) = 1 + len xs

--binary tree
data Tree a = Leaf a | Node (Tree a) a (Tree a)
                deriving (Eq, Ord, Show, Read)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
            (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)       = x == y
occurs x (Node l y r)   = x == y || occurs x l || occurs x r 

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = (flatten l) ++ [x] ++ (flatten r) 