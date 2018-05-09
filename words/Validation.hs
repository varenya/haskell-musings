module Validation where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Validation e a
  = Failure' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' r) = Success' (f r)

genValidation :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
genValidation = do
  x <- arbitrary
  y <- arbitrary
  frequency [(1, return (Failure' x)), (2, return (Success' y))]

instance (Arbitrary a,Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = genValidation

instance (Eq a,Eq b) => EqProp (Validation a b) where
     (=-=) = eq

instance (Monoid e) => Applicative (Validation e) where
  pure = Success'
  (Failure' x) <*> (Failure' y) = Failure' (x `mappend` y)
  (Failure' x) <*> _ = Failure' x
  _ <*> (Failure' x) = Failure' x
  Success' f <*> Success' x = Success' (f x)

data Errors = DividedByZero | StackOverFlow | MooglesChewedWires deriving (Eq,Show)

genErrors :: Gen Errors
genErrors = elements [DividedByZero,StackOverFlow,MooglesChewedWires]


instance Arbitrary Errors where
    arbitrary = genErrors

main :: IO ()
main = do
  let test =
        Success' ("hello", "world", "why") :: Validation [Errors] ( String
                                                                  , String
                                                                  , String)

  quickBatch $ applicative test 
  putStrLn "hello"
