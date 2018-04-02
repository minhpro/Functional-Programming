module ListExtend where

--Merge function
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

--Merge sort
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort first) (msort second)
                    where 
                        (first, second) = halve xs

-- Split a list into two halves                    
halve :: [a] -> ([a],[a])
halve xs = splitAt (div (length xs) 2) xs

--remove duplicate items from a list
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (\y -> y /= x) xs

--foldrn (+) 0 [[1,2,3], [4,5,6], [7,8,9]] = [12, 15, 18]
foldn :: (a->b->b) -> b -> [[a]] -> [b]
foldn f v [] = []
foldn f v xs = if isEmpty (head xs) then 
                    [] 
               else
                    foldr f v (map head xs) : foldn f v (map tail xs)

isEmpty :: [a] -> Bool
isEmpty xs = length xs == 0