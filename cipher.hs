module Cihper where

import           Data.Char

ceaser :: String -> Int -> String
ceaser "" _         = ""
ceaser (c:cs) count = (shift count c) : ceaser cs count

unceaser :: String -> Int -> String
unceaser "" _         = ""
unceaser (c:cs) count = (unshift count c) : unceaser cs count

shift :: Int -> Char -> Char
shift count c =
  (chr . (+ charOffset) . (\x -> (x + count - charOffset) `mod` 26) . ord) c
  where
    charOffset =
      if isUpper c
        then ord 'A'
        else ord 'a'

unshift :: Int -> Char -> Char
unshift count c =
  (chr . (+ charOffset) . (\x -> (x - count - charOffset) `mod` 26) . ord) c
  where
    charOffset =
      if isUpper c
        then ord 'A'
        else ord 'a'

type KeyWord = String

createPairs :: KeyWord -> [String] -> [(String, String)]
createPairs _ [] = []
createPairs keyWord (w:ws) =
  (w, take currentLength keyWord) : createPairs (drop currentLength keyWord) ws
  where
    currentLength = length w

getEncodedWord :: (String, KeyWord) -> String
getEncodedWord (origWord, keyWord) =
  map (\(c, k) -> shift (count k) c) $ zip origWord keyWord
  where
    count c =
      if isUpper c
        then (ord c - ord 'A')
        else (ord c - ord 'a')
    getChar = shift

ceaser2 :: KeyWord -> String -> String
ceaser2 "" _ = ""
ceaser2 keyWord inputStr = unwords $ map getEncodedWord pairs
  where
    repeatKeyWord = cycle keyWord
    pairs = createPairs repeatKeyWord (words inputStr)

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []        = False
myAny pred (x:xs) = pred x || myAny pred xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem el (x:xs)
  | el == x = True
  | otherwise = myElem el xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' _ []   = False
myElem' el lst = any (== el) lst

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []          = []
squishMap mapper (x:xs) = mapper x ++ squishMap mapper xs

squish' :: [[a]] -> [a]
squish' = squishMap id
