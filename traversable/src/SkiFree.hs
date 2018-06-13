{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data S n a =
  S (n a)
    a
  deriving (Eq, Show)

-- instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
--   arbitrary = S <$> arbitrary <*> arbitrary

-- instance (Applicative n, Testable (n Property), EqProp a) =>
--          EqProp (S n a) where
--   (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

instance (Arbitrary (n a), CoArbitrary (n a), Arbitrary a, CoArbitrary a) =>
         Arbitrary (S n a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ S (x y) y

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq

sTraversable :: S [] (Int, Int, [Int])
sTraversable = undefined

instance (Functor n) => Functor (S n) where
  fmap f (S n x) = S (fmap f n) (f x)

instance (Foldable n) => Foldable (S n) where
  foldMap f (S n x) = (foldMap f n) <> (f x)

instance (Traversable n) => Traversable (S n) where
  traverse trav (S n x) = S <$> (traverse trav n) <*> (trav x)

-- sTraversable :: S [] (Int, Int, [Int])
-- sTraversable = undefined

main :: IO ()
main = do
  quickBatch $ functor sTraversable
  quickBatch $ traversable sTraversable
--  quickBatch $ traversable (S [("a", "b", "c")] ("a", "b", "c"))
