-- List Monad
-- xs >>= f = concat (map f xs)

-- takes each number n in the argument list 
-- and generates n copies of it in ther result list
themselvesTimes :: [Int] -> [Int]
themselvesTimes xs = xs >>= copies
    where 
        copies n = replicate n n