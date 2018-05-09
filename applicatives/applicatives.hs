{-# LANGUAGE FlexibleInstances #-}

import           Control.Applicative
import           Data.List           (elemIndex)

-- instance Monoid a => Monoid (Maybe a) where
--     mempty = Nothing
--     mappend m Nothing = m
--     mappend Nothing m = m
--     mappend (Just a) (Just a') = Just (mappend a a')
-- instance Applicative Maybe where
--     pure = Just
--     Nothing <*> _ = Nothing
--     _ <*> Nothing = Nothing
--     Just f <*> Just a = Just (f a)
f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]

m x = lookup x [(4, 10), (8, 13), (1, 9001)]

added :: Maybe Integer
added = fmap (+ 3) (lookup 0 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

c :: Maybe Int
c = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = liftA2 max' x c

xs = [1, 2, 3]

ys = [4, 5, 6]

x1 :: Maybe Integer
x1 = lookup 3 $ zip xs ys

y1 :: Maybe Integer
y1 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> liftA2 (,) x1 y1

data Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

type Id = Identity

xs1 = [1, 2, 3]

xs2 = [9, 9, 9]

output = const <$> (Identity xs1) <*> (Identity xs2)

newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant {getConstant = x}) = Constant {getConstant = x}

instance (Monoid a) => Applicative (Constant a) where
  pure x = Constant {getConstant = mempty}
  Constant {getConstant = f} <*> Constant {getConstant = x} =
    Constant {getConstant = (f `mappend` x)}

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
    then Nothing
    else Just s

newtype Name =
  Name String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person =
  Person Name
         Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a

data Cow = Cow
  { name   :: String
  , age    :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight =
  liftA3 Cow (noEmpty name) (noNegative age) (noNegative weight)
