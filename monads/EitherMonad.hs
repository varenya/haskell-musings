module EitherMonad where

type Founded = Int

type Coders = Int

data SoftwareShop = Shop
  { founded     :: Founded
  , programmers :: Coders
  } deriving (Eq, Show)

data FoundError
  = NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded
                          Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 500 = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second x) = Second (f x)

instance Applicative (Sum a) where
  pure = Second
  (First x) <*> _ = First x
  _ <*> (First x) = First x
  (Second f) <*> (Second x) = Second (f x)

instance Monad (Sum a) where
  return = pure
  (First x) >>= _ = First x
  (Second x) >>= k = k x
