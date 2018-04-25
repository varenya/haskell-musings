data Wrap f a = Wrap (f a) deriving (Eq,Show)

instance (Functor f) => Functor (Wrap f) where
    fmap mapF (Wrap f)  = Wrap (fmap mapF f)