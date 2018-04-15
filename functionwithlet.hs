module FunctionWithLet where

subtract a b = a - b

add a b = a + b

divide a b = a `div` b

printInc2 n = let plusTwo = n + 2
              in print plusTwo
