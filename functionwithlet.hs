module FunctionWithLet where

subtract a b = a - b

add a b = a + b

divide a b = a `div` b

myAbs :: Integer -> Integer
myAbs x
    | x < 0  = (-x)
    | otherwise = x

printInc2 n = let plusTwo = n + 2
              in print plusTwo
