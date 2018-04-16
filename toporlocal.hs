module TopOrLocal where

topLevelFunction :: Integer -> Integer
topLevelFunction x = x + topLevelValue + woot
    where woot:: Integer
          woot = 10

topLevelValue :: Integer
topLevelValue = 5

area d = pi * (r * r)
    where r = d/2