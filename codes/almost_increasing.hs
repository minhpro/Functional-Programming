-- Given a sequence of integers as an array, determine whether it is possible to obtain a strictly increasing sequence by removing no more than one element from the array.

-- Example

-- For sequence = [1, 3, 2, 1], the output should be
-- almostIncreasingSequence(sequence) = false;

-- There is no one element in this array that can be removed in order to get a strictly increasing sequence.

-- For sequence = [1, 3, 2], the output should be
-- almostIncreasingSequence(sequence) = true.

-- You can remove 3 from the array to get the strictly increasing sequence [1, 2]. Alternately, you can remove 2 to get the strictly increasing sequence [1, 3].


almostIncreasingSequence [] = True
almostIncreasingSequence [x] = True
almostIncreasingSequence (x:y:ys) =
    if x < y then 
        almostIncrease' x (y:ys) 
    else 
        isIncrease (y:ys)

almostIncrease' :: Int -> [Int] -> Bool
almostIncrease' previous [] = True
almostIncrease' previous [x] = True
almostIncrease' previous [x,y] = True
almostIncrease' previous (x:y:z:zs) 
    | x < y = almostIncrease' x (y:z:zs)
    | x >= y && previous < y = isIncrease (y:z:zs)
    | x >= y && x < z = isIncrease (z:zs)
    | otherwise = False

isIncrease :: [Int] -> Bool
isIncrease [x] = True
isIncrease (x:y:ys) = if x < y then isIncrease (y:ys) else False