type Person = String

-- Database provides two functions
father :: Person -> Maybe Person
father "A" = Just "B"
father "B" = Just "C"
father "D" = Just "E"
father _   = Nothing
mother :: Person -> Maybe Person
mother "A" = Just "D"
mother _   = Nothing

-- Query the father of one' mother
materalGrandfather :: Person -> Maybe Person
materalGrandfather p =
    case mother p of
        Nothing -> Nothing
        Just mom -> father mom

-- Check whether both grandmothers are in the database
bothGrandfathers :: Person -> Maybe (Person, Person)
bothGrandfathers p =
    father p >>=
        (\dad -> father dad >>=
            (\gf1 -> mother p >>=
                (\mom -> father mom >>=
                    (\gf2 -> return (gf1, gf2)))))

-- Associative (m >>= f) >>= g = m >>= (\x -> f x >>= g)
bothGrandfathers2 :: Person -> Maybe (Person, Person)
bothGrandfathers2 p =
    (father p >>= father) >>=
        (\gf1 -> (mother p >>= father) >>=
            (\gf2 -> return(gf1, gf2)))

-- do Notation  
bothGrandfathers3 :: Person -> Maybe (Person, Person)
bothGrandfathers3 p = do
    dad <- father p
    gf1 <- father dad  
    mom <- mother p
    gf2 <- father mom
    return (gf1, gf2)

bothGrandfathers4 :: Person -> Maybe (Person, Person)
bothGrandfathers4 p = do
    gf1 <- father p >>= father 
    gf2 <- mother p >>= father
    return (gf1, gf2)

-- The operator (>>) called "then" is convenience implemented as
-- (>>) :: Monad m => m a -> m b -> m b
-- m >> n = m >>-= (\_ -> n)
-- (>>) sequences two monadic actions when the second action does not involve 
-- the result of the first, which is a common scenario for monads such as IO

printSomethingTwice :: String -> IO ()
printSomethingTwice str = putStrLn str >> putStrLn str

-- Monadic composition 
-- (f >=> g) >=> h = f >=> (g >=> h)
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- f >=> g = \x -> f x >>= g

-- Join function flattens a container of containers into a single container
join :: Monad m => m (m a) -> m a
join x = x >>= id

-- Exercises
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- Partially applied function functor
-- instance Functor ((->) a) where
    --fmap :: (b -> c) -> (a -> b) -> (a -> c)
    --fmap = (.)

-- Functor laws proof
-- fmap id g = id . g = g
-- => fmap id = id
-- fmap (f . g) h = (f . g) . h
-- (fmap f . fmap g) h = fmap f (fmap g h) = fmap f (g . h) = f . (g . h)
-- => fmap (f . g) = fmap f . fmap g
-- fmap (even . (+1)) 2
-- (fmap even . fmap (+1)) 2

-- Partiallly applied applicative functor
--instance Applicative ((->) a) where
    -- pure :: b -> (a -> b)
    --pure = const 
    -- (<*>) :: (a -> (b -> c)) -> (a -> b) -> (a -> c)
    --g <*> h = \x -> g x (h x)

-- (->) a Monad
--instance Monad ((->) a) where
    --(>>=) :: (a -> b) -> (b -> (a -> c)) -> (a -> c)
    --f >>= g = \x -> g (f x) x

-- Monad laws proof
-- Right unit f >>= return = f
-- f >>= return = f >>= const = \x -> const (f x) x = \x -> f x = f
-- Left unit return b >>= g = g b
-- return b >>= g = \x -> g (return b x) x = \x -> g (const b x) x
-- = \x -> g b x = g b
-- Associative (f >>= g) >>= h = f >>= (\x -> g x >>= h)
-- ((f >>= g) >>= h) x = h ((f >>= g) x) x = h (g (f x) x) x
-- (f >>= (\x -> g x >>= h)) x = (\x -> g x >> h) (f x) x = (g (f x) >> h) x
-- = h (g (f x) x) x

--ZipList
newtype ZipList a = Z [a] deriving Show
instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z [x]
    -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]

-- Exercise 7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    --fmap :: (a -> b) -> Expr a -> Expr b
    fmap g (Var x) = Var (g x)
    fmap _ (Val x) = Val x
    fmap g (Add e f) = Add (fmap g e) (fmap g f) 

instance Applicative Expr where
    --pure :: a -> Expr a
    pure = Var
    --(<*>) :: Expr (a -> b) -> Expr a -> Expr b
    _ <*> Val x = Val x
    Val x <*> _ = Val x
    Var f <*> Var x = Var (f x)
    Var f <*> Add x y = Add (fmap f x) (fmap f y)
    Add f g <*> x = Add (f <*> x) (g <*> x)

instance Monad Expr where
    --(>>=) :: Expr a -> (a -> Expr b) -> Expr b
    Val x >>= _ = Val x
    Var x >>= f = f x
    Add x y >>= f = Add (x >>= f) (y >>= f) 

-- Exercise ST in monad context
type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S x) = x

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do
        x <- st
        return (g x)

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x, s))
    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do
        f <- stf
        x <- stx
        return (f x)

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

