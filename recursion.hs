import Debug.Trace (trace)
import Data.List (intersperse)
import Data.Bool
factorial 0 = 1
factorial n =  n * factorial (n-1)


incTimes 0 b = b
incTimes n b = 1 + incTimes (n-1) b

data DividedResult = 
         Result (Integer,Integer) | DividedByZero deriving (Show)


dividedBy :: Integer -> Integer -> DividedResult
dividedBy num 0 = DividedByZero
dividedBy num denom = go num denom 0
        where go n d count 
                | (bothNeg num denom) && (posN < posD) = Result (count,n)
                | (isNeg num denom) && (posN < posD) = Result (-count,n)
                | posN < posD = Result (count,n)
                | otherwise = go (posN-posD) posD (count + 1)
                where posN = (abs n)
                      posD = (abs d)
                      isNeg numerator denominator = numerator < 0 || denominator < 0 
                      bothNeg numerator denominator = numerator < 0  && denominator < 0

sumOfN :: (Eq a, Num a) => a -> a 
sumOfN 0 = 0
sumOfN n = n + sumOfN (n-1)

multiplyBy :: (Integral a) => a -> a -> a
multiplyBy x y = go x y 0 
        where 
              go x 0 result = result 
              go x y result = go x (y-1) (result + x)

multiplyBy' :: (Integral a) => a -> a -> a
multiplyBy' x 0 = 0
multiplyBy' x y = x + multiplyBy' x (y-1)

mc91 n 
    | n > 100 = n-10
    | otherwise = mc91 $ mc91 (n+11)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10] 

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits

map3 = map (\x -> bool x (-x) (x==3)) [1..10]

zipCustom :: [a] -> [b] -> [(a,b)]
zipCustom [] _ =  []
zipCustom _ [] = []
zipCustom (x:xs) (y:ys) =  (x,y) : zipCustom xs ys

zipWithCustom :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithCustom _ _ [] =  []
zipWithCustom _ [] _ =  []
zipWithCustom applyFn (x:xs) (y:ys) = (applyFn x y) : zipWithCustom applyFn xs ys

zipFn :: a -> b -> (a,b)
zipFn x y = (x,y)

zipCustom1 :: [a] -> [b] -> [(a,b)]
zipCustom1 = zipWith  zipFn

