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
