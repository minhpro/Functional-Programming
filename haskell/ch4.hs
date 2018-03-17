--exercise 1. split a list into two halves
halve :: [a] -> ([a],[a])
halve xs = splitAt (div (length xs) 2) xs

--exercise 2. return the third element in al ist
third :: [a] -> a
--method 1. using head and tail
--third xs = head (tail (tail xs))
--method 2. using list indexing !!
--third xs = xs !! 2
--method 3. pattern matching
third (_:_:x:_) = x

--exercise 3. 
safetail :: [a] -> [a]
-- safetail xs = if null xs then xs else tail xs --condition expression
-- safetail xs | null xs = xs
--             | otherwise = tail xs --guarded equation
safetail [] = []
safetail (x:xs) = xs --pattern matching
--exercise 8. Luhn algorithm
luhnDouble :: Int -> Int
luhnDouble n = if n * 2 > 9 
                    then n * 2 - 9 
                    else n * 2

luhn :: [Int] -> Bool



