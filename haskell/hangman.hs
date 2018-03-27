import System.IO

hangman :: IO ()
hangman = do putStrLn "Think of a word:"
            word <- sgetLine
            putStrln "Try to guess it:"
            play word

--reads a string from keyboard, echoes each character as a dash smbol '_"
sgetLine :: IO string
sgetLine = do x <- getCh
                if x == '\n' then
                    do putChar x
                        return []
                else 
                    do putChar '_'
                        xs <- sgetLine
                        return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False 
            x <- getChar
            hSetEcho stdin True
            return x


