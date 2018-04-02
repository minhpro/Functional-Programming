qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

factorial :: Int -> Int
factorial 0 = 1
factorial n | n < 0 = n * factorial (n+1)
            | otherwise = n * factorial (n-1)
        
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

expo :: Int -> Int -> Int
expo 0 _ = 0
expo _ 0 = 1
expo x y = x * expo x (y-1)

euclid :: Int -> Int -> Int
euclid a 0 = a
euclid 0 b = b
euclid a b  | a >= b    = euclid (mod a b) b
            | otherwise = euclid a (mod b a)

concat' :: [[a]] -> [a]
concat' []      = []
concat' (x:xs)  = x ++ concat' xs 

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' 1 x = [x]
replicate' n x = x:(replicate' (n-1) x)

nth :: [a] -> Int -> a
nth (x:_) 0 = x
nth (_:xs) n = nth xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
  | x == y = True
  | otherwise = elem' x ys

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
        | x <= y = x : merge xs (y:ys)
        | otherwise = y: merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = (take (div (length xs) 2) xs, drop (div (length xs) 2) xs)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort l) (msort r)
        where (l,r) = halve xs

foldl' :: Num a => (b -> a -> a) -> [b] -> a