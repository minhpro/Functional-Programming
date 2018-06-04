data ST s a = ST (s -> (a, s)) 

app :: ST s a -> s -> (a, s)
app (ST g) = g

instance Functor (ST s) where
    -- fmap :: (a -> b) -> ST s a -> ST s b
    fmap g (ST h) = ST (\s -> let (x, s') = h s in (g x, s'))

instance Applicative (ST s) where
    -- pure :: a -> ST s a
    pure x = ST (\s -> (x, s))
    -- (<*>) :: ST s (a -> b) -> ST s a -> ST s b
    (ST h) <*> (ST g) = ST $ \s ->  let (f, s') = h s 
                                        (x, s'') = g s'
                                    in (f x, s'')

instance Monad (ST s) where
    -- (>>=) :: ST s a -> (a -> ST s b) -> ST s b
    (ST h) >>= f = ST $ \s -> let (a, s') = h s
                                  (ST g) = f a
                              in g s'

type Stack = [Int]

pop :: ST Stack Int
pop = ST $ \(x:xs) -> (x, xs)

push :: Int -> ST Stack ()
push x = ST $ \xs -> ((), x:xs)

stackManip :: ST Stack Int
stackManip = do
    push 3
    pop
    pop