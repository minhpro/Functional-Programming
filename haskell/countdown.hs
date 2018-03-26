data Op = Add | Sub | Mul | Div
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

--validate if the application of an operator to two positive natural 
--gives another positive natural
valid :: Op -> Int -> Int -> Bool
--valid Add _ _ = True
valid Add x y = x <= y
valid Sub x y = x > y
-- valid Mul _ _ = True
valid Mul x y = x /= 1 && y /= 1 && x <= y
-- valid Div x y = x `mod` y == 0
valid Div x y = y /= 1 && x`mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

--Numeric expressions
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
                        where
                            brak (Val n) = show n
                            brak e       = "(" ++ show e ++ ")"

--The list of values in an expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

--eval return result, a singleton list denotes success, and the empty list denotes failure
eval :: Expr -> [Int]
eval (Val n) = [n]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

--return all subsequences
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
                where yss = subs xs

--return all possible ways of inserting a new element into a list
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

--return all permutations of a list
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

--return all ways to select zero or more element in any order from a list
--all permutations of all subsequences
choices :: [a] -> [[a]]
-- choices = concat . map perms . subs
choices xs = [x | y <- subs xs, x <- perms y] --Exercise 1

--return true if solution is satisfy problem
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

--return all ways to split a list into two non-empty list
split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls, rs) | (ls,rs) <- split xs]

--return all expressions whose list of values is precisely a given list
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l        <- exprs ls,
                r        <- exprs rs,
                e        <- combine l r]

--combine expressions                
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

--return a list of all solutions
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

--improve performance by rejecting earlier invalid expressions
type Result = (Expr,Int)
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [e | (ls, rs) <- split ns,
                  l        <- results ls,
                  r        <- results rs,
                  e        <- combine' l r]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, n') <- results ns', n' == n]

--Exercise 6.b find the nearest solution
maxValue = maxBound :: Int
maxResult = (Val maxValue, maxValue)
nearestSolution :: [Int] -> Int -> Result
nearestSolution ns n = findSolution [r | ns' <- choices ns, r <- results ns'] 
                                    n 
                                    maxResult

findSolution :: [Result] -> Int -> Result -> Result
findSolution [] _ c = c --c is the current nearest solution so far
findSolution ((e,x):xs) n (c, m) | abs (x-n) < abs (m-n) = findSolution xs n (e,x)
                                 | otherwise = findSolution xs n (c,m)

--nearestSolution [1,3,7,10,25,50] 831 = (7+((1+10)*(25+50)),832)

main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)