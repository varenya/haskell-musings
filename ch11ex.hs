import           Data.Char
import           Data.List
import           Data.List.Split

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf subSeq@(x:xs) seq@(y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf xs seq && isSubseqOf subSeq ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = (map capitalize . words)
  where
    capitalize string@(c:cs) = (string, (toUpper c : cs))

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:cs)
  | c == ' ' = c : capitalizeWord cs
  | otherwise = toUpper c : cs

capitalizeParagraph :: String -> String
capitalizeParagraph = (intercalate "." . map capitalizeWord . splitOn ".")
