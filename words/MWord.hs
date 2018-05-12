module MWord where


class MWord m where
    mpull :: (a -> m b) -> m a -> m b
    mpush :: a -> m a
    
    mtrans :: (a->b) -> m a -> m b
    mtrans f =  mpull (mpush . f)
                    