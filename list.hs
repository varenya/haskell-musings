import Data.Char

myWords :: String -> [String]
myWords "" = []
myWords inputStr = takeWhile (/= ' ') inputStr : myWords (dropCustom inputStr)
                 where dropCustom = (drop 1 . dropWhile (/= ' '))


firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: Char -> String -> [String]
myLines _ "" = []
myLines sep lines = takeWhile (/= sep) lines : myLines sep (dropCustom lines)
                where dropCustom = (drop 1 . dropWhile (/= sep))

filterLower :: String -> String
filterLower = filter isUpper

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs

toUpperCase :: String -> String
toUpperCase [] = ""
toUpperCase (c:cs) = toUpper c : toUpperCase cs

headUpper :: String -> Char
headUpper []   = ' '  
headUpper str  = toUpper (head str)