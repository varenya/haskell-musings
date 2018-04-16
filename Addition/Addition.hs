module Addition where

import           Test.Hspec
import Test.QuickCheck

data DividedResult = 
         Result (Integer,Integer) | DividedByZero deriving (Eq,Show)


dividedBy :: Integer -> Integer -> DividedResult
dividedBy _ 0 = DividedByZero
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

main :: IO ()
main =
  hspec $ do
    describe "Addition" $ do
        it "15 divided by 5 is 3" $ do 
            dividedBy 15 5 `shouldBe` Result (3,0)
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
