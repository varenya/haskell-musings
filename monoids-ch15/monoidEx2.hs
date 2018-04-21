import           Data.Semigroup
import           Test.QuickCheck

data Trivial =
  Trivial
  deriving (Eq, Show)

data Two a b =
  Two a
      b
  deriving (Eq, Show)

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

data Validation a b
  = VFailure a
  | VSuccess b
  deriving (Eq, Show)

newtype Identity a =
  Identity a
  deriving (Eq, Show)

newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

newtype Combine a b = Combine
  { unCombine :: (a -> b)
  }

newtype Comp a = Comp
  { unComp :: (a -> a)
  }

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)

combineMappend :: (Semigroup b) => (a -> b) -> (a -> b) -> a -> b
combineMappend f g arg = f arg <> g arg

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Semigroup a => Semigroup (Validation a b) where
  (VFailure x) <> (VFailure y) = VFailure (x <> y)
  (VSuccess x) <> _ = VSuccess x
  (_) <> VSuccess y = VSuccess y

instance Semigroup b => Semigroup (Combine a b) where
  (Combine {unCombine = f}) <> (Combine {unCombine = g}) =
    Combine {unCombine = combineMappend f g}

instance Semigroup a => Semigroup (Comp a) where
  Comp {unComp = f} <> Comp {unComp = g} = Comp {unComp = combineMappend f g}

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (Three x1 y1 z1) <> (Three x2 y2 z2) = Three (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) =
    Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (Fst _) <> (Fst y) = Fst y
  (Snd x) <> _ = Snd x
  _ <> (Snd y) = Snd y

instance Semigroup BoolConj where
  (BoolConj _) <> (BoolConj False) = BoolConj False
  (BoolConj False) <> (BoolConj _) = BoolConj False
  (BoolConj True) <> (BoolConj True) = BoolConj True

instance Semigroup BoolDisj where
  (BoolDisj True) <> (BoolDisj _) = BoolDisj True
  (BoolDisj _) <> (BoolDisj True) = BoolDisj True
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

genFour ::
     (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
genFour = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

genOrData :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
genOrData = do
  a <- arbitrary
  b <- arbitrary
  elements [Fst a, Snd b]

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = genThree

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = genFour

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj True, BoolConj False]

instance Arbitrary BoolDisj where
  arbitrary = elements [BoolDisj True, BoolDisj False]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = genOrData

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc
   = Identity String -> Identity String -> Identity String -> Bool

type TwoString = Two String String

type TwoAssoc = TwoString -> TwoString -> TwoString -> Bool

type ThreeString = Three String String String

type ThreeAssoc = ThreeString -> ThreeString -> ThreeString -> Bool

type FourString = Four String String String String

type FourAssoc = FourString -> FourString -> FourString -> Bool

type BoolAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type OrString = Or String String

type OrAssoc = OrString -> OrString -> OrString -> Bool

main :: IO ()
main = do
  putStrLn "Trivial Association"
  quickCheck (semigroupAssoc :: TrivialAssoc)
  putStrLn "Identity Association"
  quickCheck (semigroupAssoc :: IdentityAssoc)
  putStrLn "Two Association"
  quickCheck (semigroupAssoc :: TwoAssoc)
  putStrLn "Three association"
  quickCheck (semigroupAssoc :: ThreeAssoc)
  putStrLn "Four association"
  quickCheck (semigroupAssoc :: FourAssoc)
  putStrLn "BoolConj Association"
  quickCheck (semigroupAssoc :: BoolAssoc)
  putStrLn "BoolDisj Association"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  putStrLn "Or Association"
  quickCheck (semigroupAssoc :: OrAssoc)

runValidation :: IO ()
runValidation = do
  let failure :: String -> Validation String Int
      failure = VFailure
  let success :: Int -> Validation String Int
      success = VSuccess
  print $ failure "hello" <> failure "world"
  print $ success 1 <> success 2
  print $ failure "why" <> success 2
