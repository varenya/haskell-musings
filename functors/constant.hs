newtype Constant a b = Constant { getConstant :: a} deriving (Eq,Show)

instance Functor (Constant a) where
    fmap _ (Constant v) = Constant v

