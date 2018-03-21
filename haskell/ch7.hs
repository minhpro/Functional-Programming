twice :: (a -> a) -> a -> a
twice f x = f (f x)

--1.List comprehension using map and filter
list_cons :: (a -> b) -> (a -> Bool) -> [a] -> [b]
list_cons f p = (map f) . (filter p)

--2.List operations
all' :: (a -> Bool) -> [a] -> Bool
--all' p = foldr (\x y -> p x && y) True 
--all' p = foldl (\x y -> x && p y) True
all' p [] = True
all' p (x:xs)   | p x       = all' p xs
                | otherwise = False 

any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs)   | p x       = True
                | otherwise = any' p xs

--Select elements from a list while they satisfy a predicate
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs
                            else []

--Remove elements from a list while they satisfy a predicate
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x : dropWhile' p xs

--3 Redefine map and filter using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x y -> if p x then x : y else y) []

--4 Convert decimal number to Int
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x * 10 + y) 0

--5 Convert functions on pairs to a curried function and conversely.
curry' :: ((a, b) -> c) -> a -> b -> c 
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f p = f (fst p) (snd p)

--6 Unfold function
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x  | p x       = []
                | otherwise = h x : unfold p h t (t x) 

--int2bin 13 = [1,1,0,1]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

--chop4 [1,0,1,1,1,1,0,0] = [[1,0,1,1],[1,1,0,0]]
chop4 :: Eq a => [a] -> [[a]]
chop4 = unfold (== []) (take 4) (drop 4)

--8 altMap function
--altMap (+10) (+100) [1,2,3,4] = [11, 102, 13, 104]
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs