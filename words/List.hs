module List where

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil Nil       = Nil
  mappend Nil ys        = ys
  mappend (Cons x xs) y = Cons x (mappend xs y)

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> list = (fmap f list) <> (fs <*> list)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  list >>= f = concat' $ fmap f list

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

take' :: Int -> List a -> List a
take' 0 _             = Nil
take' _ Nil           = Nil
take' num (Cons x xs) = Cons x $ take' (num - 1) xs

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
        in take' 3000 l
      ys' =
        let (ZipList' l) = ys
        in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (fmap f xs)

genList :: Arbitrary a => Gen (List a)
genList = do
  a <- arbitrary
  l <- genList
  frequency [(1, return Nil), (2, return (Cons a l))]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let l = xs
        in take' 1000 l
      ys' =
        let l = ys
        in take' 1000 l

instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil)
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ (zipListWith fs xs)

zipListWith :: List (a -> b) -> List a -> List b
zipListWith _ Nil                    = Nil
zipListWith Nil _                    = Nil
zipListWith (Cons f Nil) (Cons x xs) = Cons (f x) (pure f <*> xs)
zipListWith (Cons f fs) (Cons x Nil) = Cons (f x) (fs <*> pure x)
zipListWith (Cons f fs) (Cons x xs)  = Cons (f x) (zipListWith fs xs)

main :: IO ()
main = do
  putStrLn "List test"
  let z1 = (Cons ("A", "b", "c") Nil)
  quickBatch $ applicative z1
  putStrLn "ZipList test"
  quickBatch $ applicative (ZipList' (Cons ("A", "b", "b") Nil))
  putStrLn "\nTesting Monad Laws"
  quickBatch $ monad (Cons ("a", "b","c") Nil)
