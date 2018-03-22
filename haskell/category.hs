import Data.Void
--non-terminating computation. the halting problem
f :: Bool -> Bool
f = undefined
--f x = undefined

--Void type, type has no value
absurd' :: Void -> Int
absurd' _ = 10

absurd'' :: a -> Void
absurd'' _ = undefined

--Singleton types, types that have only one value
f44 :: () -> Int
f44 () = 44

fInt :: Int -> ()
fInt _ = ()

unit :: a -> ()
unit _ = ()