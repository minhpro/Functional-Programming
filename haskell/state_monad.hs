-- Randomness
-- Rolling dice

import Control.Applicative
import System.Random

rollDiceIO :: IO (Int, Int)
rollDiceIO = liftA2 (,) (randomRIO (1,6)) (randomRIO (1,6))

-- liftA2 f u v = f <$> u <*> v

