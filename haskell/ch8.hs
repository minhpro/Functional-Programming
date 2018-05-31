import ListExtend
import Data.List

type Pos = (Int,Int)
type Trans = Pos -> Pos

type Pair a = (a,a)

type Assoc k v = [(k,v)]

find' :: Eq k => k -> Assoc k v -> v
find' k t = head [v | (k',v) <- t, k == k']

--[(1,2), (2,3), (2,4), (3,5), (3,6)] => [(1,2), (2,7), (3,11)]
reduceByKey :: Ord k => (v -> v -> v) -> Assoc k v -> Assoc k v
reduceByKey f xs = reduceByKey' f (sortOn fst xs)

--Assume is sorted
reduceByKey' :: Eq k => (v -> v -> v) -> Assoc k v -> Assoc k v
reduceByKey' _ [] = []
reduceByKey' _ [x] = [x]
reduceByKey' f ((k,v):(k',v'):xs) = 
    if k == k' then reduceByKey' f ((k, f v v'):xs)
    else (k,v) : reduceByKey' f ((k',v'):xs)

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
                -- deriving (Eq, Ord, Show, Read)

len :: List a -> Int 
len Nil = 0
len (Cons _ xs) = 1 + len xs

--binary tree
data Leaf a = TreeLeaf a

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
                | Or Prop Prop | Equal Prop Prop
                    deriving (Eq, Ord, Show, Read)

type Subst = Assoc Char Bool

--evaluate a Proposition given a Subst
eval :: Subst -> Prop -> Bool 
eval _ (Const b) = b 
eval s (Var x) = find' x s 
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q 
eval s (Or p q) = eval s p || eval s q
eval s (Equal p q) = eval s p == eval s q

--get the list of all Vars from the Prop
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p 
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Equal p q) = vars p ++ vars q

--generate subsitutions
bools :: Int -> [[Bool]]
bools 1 = [[False], [True]]
bools n = map (False:) bss ++ map (True:) bss
            where bss = bools (n-1)

--Generating list of subsitutions for a Proposition
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where vs = rmdups (vars p)

p2 = Imply (And (Var 'A') (Equal (Var 'B') (Var 'C'))) (Or (Var 'A') (Var 'C'))

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p] 

--Abstract machine
--Arithmetic expressions from integers and an addition operator
data Expr = Val Int | Add Expr Expr | Mult Expr Expr
                deriving (Eq, Ord, Show, Read)

value :: Expr -> Int
value (Val n) = n 
value (Add x y) = value x + value y
value (Mult x y) = value x * value y

--control stacks for the abstract machine, which comprise a list of operations
--to be performed by the machine after the current evaluation has been completed
type Cont = [Op]

data Op = EVALADD Expr | EVALMULT Expr | ADD Int | MULT Int

eval' :: Expr -> Cont -> Int
eval' (Val n) c = exec c n
eval' (Add x y) c = eval' x (EVALADD y : c)
eval' (Mult x y) c = eval' x (EVALMULT y : c)

exec :: Cont -> Int -> Int
exec [] n = n 
exec (EVALADD y : c) n = eval' y (ADD n : c)
exec (EVALMULT y : c) n = eval' y (MULT n : c)
exec (ADD n : c) m = exec c (n+m)
exec (MULT n : c) m = exec c (n*m)

value' e = eval' e []

e1 = (Add (Add (Val 2) (Val 3)) (Mult (Val 4) (Val 5)))

--Exercise 1. 
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ m) (Succ n) = add (Succ n) (mult m (Succ n))

--Exercise 2.
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = (compare x y) == EQ
occurs' x (Node l y r)  | c == EQ = True
                        | c == LT = occurs' x l
                        | otherwise = occurs' x r 
                            where c = compare x y

--Exercise 3.
data BTree a = BLeaf a | BNode (BTree a) (BTree a)
                deriving (Eq, Ord, Show, Read)

leavesCount :: BTree a -> Int
leavesCount (BLeaf _) = 1
leavesCount (BNode l r) = leavesCount l + leavesCount r 

balanced :: BTree a -> Bool 
balanced (BLeaf _) = True
balanced (BNode l r) = (leavesCount l == leavesCount r)
                        && balanced l && balanced r

b1 = BNode (BNode (BLeaf 1) (BLeaf 2)) (BNode (BLeaf 3) (BLeaf 4))

--Exercise 4.
balance :: [a] -> BTree a
balance [x] = BLeaf x
balance xs = BNode (balance first) (balance second)
                where (first, second) = halve xs

--Exercise 5.
-- folde f g replace Val with f, Add with g
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

--Exercise 6.
evalexpr :: Expr -> Int
evalexpr e = folde id (+) e

size :: Expr -> Int
size e = folde (\_ -> 1) (+) e

--Exercise 7.
data Maybe' a = Nothing' | Just' a

instance Eq a => Eq (Maybe' a) where
    Nothing' == Nothing' = True
    Just' x == Just' y = x == y
    Nothing' == Just' _ = False
    Just' _ == Nothing' = False

-- instance Eq a => Eq [a] where
--     [] == [] = True
--     [] == (x:xs) = False
--     (x:xs) == [] = False
--     (x:xs) == (y:ys) = x == y && xs == ys
instance Eq a => Eq (List a) where
    Nil == Nil = True
    Nil == (Cons _ _) = False
    (Cons _ _) == Nil = False
    (Cons x xs) == (Cons y ys) = x == y && xs == ys

--Exercise 8.--see above
--Exercise 9.--see above
