import Ch4

--Merge function
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

--Merge sort function
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort first) (msort second)
                    where 
                        (first, second) = halve xs