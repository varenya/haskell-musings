{-# LANGUAGE FlexibleInstances #-}

import           GHC.Arr

data BoolAndSomethingElse a
  = False' a
  | True' a

instance Functor BoolAndSomethingElse where
  fmap f (False' x) = (False' (f x))
  fmap f (True' y)  = (True' (f y))

data BoolAndMaybeSomethingElse a
  = Falsish
  | Truish a

instance Functor BoolAndMaybeSomethingElse where
  fmap _ (Falsish)  = Falsish
  fmap f (Truish x) = Truish (f x)

-- Not doable has kind (* -> *) -> *
newtype Mu f = Inf
  { outF :: f (Mu f)
  }

-- Not doable since it has kind *
data D =
  D (Array Word Word)
    Int
    Int

data Sum a b
  = First a
  | Second b

instance Functor (Sum e) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

data Company a b c
  = DeepBlue a
             c
  | Something b

instance Functor (Company e e') where
  fmap _ (Something b)  = (Something b)
  fmap f (DeepBlue a c) = DeepBlue a (f c)

data More a b
  = R a
      b
      a
  | L b
      a
      b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (R x y z) = R x (f y) z
  fmap f (L x y z) = L (f x) y (f z)

data Quant a b
  = Finance
  | Desk a
  | Bloor b

instance Functor (Quant x) where
  fmap _ Finance   = Finance
  fmap _ (Desk x)  = Desk x
  fmap f (Bloor x) = Bloor (f x)

-- data K a b = K a
instance Functor (K a) where
  fmap _ (K x) = K x

data Tuple a b =
  Tuple a
        b
  deriving (Eq, Show)

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

newtype K a b =
  K a

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut fType) = LiftItOut (fmap f fType)

data Parappa f g a =
  DaWrappa (f a)
           (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa (fValue) (gValue)) = DaWrappa (fmap f fValue) (fmap f gValue)

data IgnoreOne f g a b =
  IngoringSomething (f a)
                    (g b)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IngoringSomething fValue gValue) =
    IngoringSomething fValue (fmap f gValue)

data Notorious g o a t =
  Notorious (g o)
            (g a)
            (g t)
  deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious oValue aValue tValue) =
    Notorious oValue aValue (fmap f tValue)

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat            = NoGoat
  fmap f (OneGoat x)       = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a
  = Halt
  | Print String
          a
  | Reading (String -> a)

-- f :: a -> b
-- g :: String -> a
instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Reading g) = Reading (f . g)