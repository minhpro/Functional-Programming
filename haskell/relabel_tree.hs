type State = Int

newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
   -- fmap :: (a -> b) -> ST a -> ST b
   fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
   -- pure :: a -> ST a
   pure x = S (\s -> (x,s))

   -- (<*>) :: ST (a -> b) -> ST a -> ST b
   stf <*> stx = S (\s ->
      let (f,s')  = app stf s
          (x,s'') = app stx s' in (f x, s''))

instance Monad ST where
   -- (>>=) :: ST a -> (a -> ST b) -> ST b
   st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')


data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

relabel :: Tree a -> Int -> (Tree Int, Int)
relabel (Leaf _) n = (Leaf n, n+1)
relabel (Node l r) n = (Node l' r', n'')
    where 
        (l', n') = relabel l n
        (r', n'') = relabel r n'

tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- a state transformer returns the current state as its result 
-- and the next integer as the new state
fresh :: ST Int
fresh = S (\n -> (n, n+1))

-- relabel the applicative style
alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

