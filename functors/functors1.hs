replaceWithP = const 'p'

data FixMePls a = FixMe | Pls a deriving (Eq,Show)

data Two a b = Two a b deriving (Eq,Show)

data Or a b =  First a | Second b deriving (Eq,Show)

newtype Identity a = Identity a deriving (Eq,Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

data Pair a = Pair a a  deriving (Eq,Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

data Three a b c = Three a b c deriving (Eq,Show)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

instance Functor FixMePls  where
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)

instance Functor (Or a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

class Ping f where
    pong :: (a -> b) -> f a -> f b

instance Ping [] where
    pong = map

instance Ping (Either t) where 
    pong _ (Left x) = Left x
    pong f (Right y) = Right (f y)

instance Ping ((->) t) where
    -- pong :: (a -> b) -> (t -> a) -> (t -> b)
    pong f g = f . g

incMaybe :: (Num a) => Maybe a -> Maybe a
incMaybe  = fmap (+1) 

liftedInc :: (Functor f,Num a) => f a -> f a
liftedInc = fmap (+1)

liftedShow :: (Functor f,Show a) => f a -> f String 
liftedShow = fmap show

data Possibly a = LolNope | Yeppers a deriving (Eq,Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers x) = Yeppers (f x)

applyIfYeppers :: (a -> b) -> Possibly a -> Possibly b
applyIfYeppers f = fmap f

liftedIncEither :: Num a => Either e a -> Either e a
liftedIncEither = fmap (+1)