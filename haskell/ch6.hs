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