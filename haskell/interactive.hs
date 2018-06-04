import Text.Read
interactiveDoubling = do
    putStrLn "Choose a number:"
    s <- getLine
    let mx = readMaybe s :: Maybe Double 
    case fmap (*2) mx of
        Just x -> putStrLn ("The double of your number is " ++ show (2*x)) 
        Nothing -> do
            putStrLn "This is not a valid number. Retrying..."
            interactiveDoubling

interactiveSumming = do
    putStrLn "Choose two numbers:"
    sx <- getLine
    sy <- getLine
    let mx = readMaybe sx :: Maybe Double
        my = readMaybe sy 
    case (+) <$> mx <*> my of
        Just z -> putStrLn ("The sum of your numbers is " ++ show z)
        Nothing -> do 
            putStrLn "Invalid numbers. Retrying..."
            interactiveSumming

interactiveConcatenating = do
    putStrLn "Choose two strings:"
    sz <- (++) <$> getLine <*> getLine
    putStrLn "Let's concatenate them:"
    putStrLn sz


-- (*>) :: Applicative f => f a -> f b -> f b
-- u *> v = (¥_ y -> y) <$> u <*> v

interactiveConcatenating2 = do
    sz <- putStrLn "Choose two strings:" *> ((++) <$> getLine <*> getLine)
    putStrLn "Let's concatenate them:" *> putStrLn sz