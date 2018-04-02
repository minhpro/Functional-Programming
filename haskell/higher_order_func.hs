import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
-- bin2int bits = sum [w*b | (w,b) <- zip weights bits]
                -- where weights = iterate (*2) 1

bin2int = foldr (\x y -> x + 2*y) 0

twice f = f . f