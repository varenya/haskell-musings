import Control.Monad
import           System.Exit   (exitSuccess)
import Data.Char (toLower)

palindrome :: IO ()
palindrome = forever $
    do 
        line1 <- getLine
        case (toLowerCase line1 == toLowerCase (reverse line1)) of
            True -> putStrLn "It's a palindrome"
            False -> putStrLn "Nope!"
        return ()
    where 
        toLowerCase = map toLower
