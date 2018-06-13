import           Data.Foldable
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

data Big a b =
  Big a
      b
      b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Monoid a => Applicative (Big a) where
  pure x = Big mempty x x
  (Big a f g) <*> (Big b y z) = Big (a <> b) (f y) (g z)

instance Foldable (Big a) where
  foldMap f (Big _ y z) = (f y) <> (f z)

instance Traversable (Big a) where
  traverse f (Big x y z) = Big <$> pure x <*> (f y) <*> (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Big x y z

instance (Eq a, Eq b) => EqProp (Big a b) where
   (=-=) = eq

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show)

genOnly :: Arbitrary a => Gen (Optional a)
genOnly = do
  a <- arbitrary
  return (Yep a)

genOptional :: Arbitrary a => Gen (Optional a)
genOptional = do
  x <- arbitrary
  frequency [(1, return Nada), (2, return (Yep x))]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = genOptional

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
  foldr _ z Nada    = z
  foldr f z (Yep x) = f x z
  foldl _ z Nada    = z
  foldl f z (Yep x) = f z x
  foldMap _ Nada    = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep x) = Yep <$> f x

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\c a -> (c == x) || a) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr go Nothing
  where
    go x Nothing  = Just x
    go y (Just x) = Just (min x y)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr go Nothing
  where
    go x Nothing  = Just x
    go y (Just x) = Just (max x y)

null' :: (Foldable t) => t a -> Bool
null' l =
  case (toList l) of
    [] -> True
    _  -> False

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ a -> a + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\c a -> f c <> a) mempty

main :: IO ()
main = do
  putStrLn "Optional :"
  quickBatch $ traversable (Yep ("a", "b", "c"))
  putStrLn ""
  putStrLn "Identity"
  quickBatch $ traversable (Identity ("a", "b", "c"))
  putStrLn ""
  putStrLn "Big Traversable :"
  quickBatch $ traversable (Big "a" ("a", "b", "c") ("a", "b", "c"))
