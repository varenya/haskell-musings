stops = "pbtdkg"
vowels = "aeious"

allStopVowels :: [(Char,Char,Char)]
allStopVowels = [ (s,v,s') | s <- stops , v <- vowels , s' <- stops]

allVowelsP :: [(Char,Char,Char)]
allVowelsP = filter (\(s,_,_) -> s == 'p') allStopVowels

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny applyFn = (myOr . map applyFn)

myElem :: Eq a => a -> [a] -> Bool
myElem elem = foldr (\c a -> (c == elem) || a) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\c a -> f c : a) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\c a -> if f c then c : a else a) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\current accumulated -> f current ++ accumulated) []

squish' :: [[a]] -> [a]
squish' = squishMap id

checkFunc :: (a -> a -> Ordering) -> a -> a -> a
checkFunc compFunc current accum = 
    case compFunc current accum of 
            LT -> accum
            GT -> current
            EQ -> accum


myMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBy _ [] = Nothing
myMaximumBy compFunction (x:xs) = Just (foldr (checkFunc compFunction) x xs)


