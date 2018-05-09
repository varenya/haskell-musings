module Exercises where

import Control.Applicative (liftA3)

stops :: String
stops = "pbtkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] ->[(a,b,c)]
combos = liftA3 (,,) 