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

--Tautology checker
data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop
                deriving (Eq, Ord, Show, Read)

type Subst = Assoc Char Bool

--evaluate a Proposition given a Subst
eval :: Subst -> Prop -> Bool 
eval _ (Const b) = b 
eval s (Var x) = find x s 
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

--get the list of all Vars from the Prop
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p 
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

--remove duplicate items from a list
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (\y -> y /= x) xs

--generate subsitutions
bools :: Int -> [[Bool]]
bools 1 = [[False], [True]]
bools n = map (False:) bss ++ map (True:) bss
            where bss = bools (n-1)

--Generating list of subsitutions for a Proposition
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where vs = rmdups (vars p)

p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p] 

--Abstract machine
--Arithmetic expressions from integers and an addition operator
data Expr = Val Int | Add Expr Expr
                deriving (Eq, Ord, Show, Read)

value :: Expr -> Int
value (Val n) = n 
value (Add x y) = value x + value y

--control stacks for the abstract machine, which comprise a list of operations
--to be performed by the machine after the current evaluation has been completed
type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n 
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n+m)
