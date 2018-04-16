{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheType where

example = 1

c :: a -> b -> a
c x y = x

co :: (b -> c) -> (a -> b) -> a -> c
co y2z x2y x = (y2z (x2y x))

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = (g (f x))

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e  = w . q

