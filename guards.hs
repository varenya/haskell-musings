myAbs :: Integer -> Integer
myAbs x
    | x < 0  = (-x)
    | otherwise = x


avgGrade :: (Fractional a , Ord a ) => a -> Char
avgGrade x  
         | y >= 0.9  = 'A'
         | y >= 0.8  = 'B'
         | y >= 0.59 = 'D'
         | y < 0.59  = 'F'
         | y >= 0.7  = 'C'
         where y = x / 100

pal :: [Char] -> Bool
pal xs 
    | xs == reverse xs = True
    | otherwise        = False



tensDigit :: Integral a => a -> a 
tensDigit =  snd . (flip divMod) 10

foldBool :: a -> a -> Bool -> a
foldBool x y z = 
    case z of 
        True -> x
        False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y z 
            | z = x
            | otherwise = y

g :: (a -> b) -> (a,c) -> (b,c)
g f (x,y) = (f x, y)