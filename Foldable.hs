import Data.Monoid
import Data.Foldable

data Identity a = Identity a deriving (Eq,Show)


instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

data Optional a = Nada | Yep a deriving (Eq,Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
    foldr _ z  Nada = z
    foldr f z (Yep x) = f x z

    foldl _ z Nada = z
    foldl f z (Yep x) = f z x

    foldMap _ Nada = mempty
    foldMap f (Yep x) = f x

instance Traversable Optional where
    traverse f Nada = pure Nada
    traverse f (Yep x) = Yep <$> f x


sum' :: (Foldable t, Num a) => t a -> a
sum'  = getSum . foldMap Sum 

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) =>  a -> t a -> Bool
elem' x = foldr (\c a -> (c == x) || a) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' =  foldr go Nothing
     where
        go x Nothing = Just x
        go y (Just x) = Just (min x y)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr go Nothing
    where
        go x Nothing = Just x
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
fold'  =  foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\c a -> f c <> a) mempty

