module EndExercises where

import           Control.Monad
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure = (\_ -> NopeDotJpg)
  (NopeDotJpg) <*> (NopeDotJpg) = NopeDotJpg

instance Monad Nope where
  return = pure
  (NopeDotJpg) >>= _ = NopeDotJpg

data AnotherEither a b
  = Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (AnotherEither a) where
  fmap _ (Left' a)  = Left' a
  fmap f (Right' b) = Right' (f b)

instance Applicative (AnotherEither a) where
  pure = Right'
  (Left' x) <*> _ = Left' x
  _ <*> (Left' x) = Left' x
  (Right' f) <*> (Right' x) = Right' (f x)

instance Monad (AnotherEither a) where
  return = pure
  (Left' x) >>= _ = Left' x
  (Right' x) >>= f = f x

genAnotherEither :: (Arbitrary a, Arbitrary b) => Gen (AnotherEither a b)
genAnotherEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left' a, Right' b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (AnotherEither a b) where
  arbitrary = genAnotherEither

instance (Eq a, Eq b) => EqProp (AnotherEither a b) where
  (=-=) = eq

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (Identity x) >>= f = f x

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f =
  let val = fmap (: []) (f x)
  in liftM2 (++) val (meh xs f)

main :: IO ()
main = do
  let trigger :: AnotherEither Int (String, String, String)
      trigger = Right' ("a", "b", "c")
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  putStrLn ""
  putStrLn "Identity Tests:"
  let identityTrigger :: Identity (String, String, String)
      identityTrigger = undefined
  quickBatch $ functor identityTrigger
  quickBatch $ applicative identityTrigger
  quickBatch $ monad identityTrigger
