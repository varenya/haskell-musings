module StringExercise where

identity :: String -> String
identity str = str

getLastChar :: String -> Char
getLastChar = last . head . words 

getLastWord :: String -> String
getLastWord = last . words

getThirdChar :: String -> Char
getThirdChar = (!! 2)

letterIndex :: Int -> Char
letterIndex  x = "Curry is awesome!" !! x

rvrs :: String -> String
rvrs str = 
    let wordsStr = words str
    in unwords $ concat [(drop 2 wordsStr), (drop 1 (take 2 wordsStr)) ,(take 1 wordsStr)]