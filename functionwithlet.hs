module FunctionWithLet where

subtract a b = a - b

add a b = a + b

printInc2 n = let plusTwo = n + 2
              in print plusTwo
