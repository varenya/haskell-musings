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

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (Three x1 y1 z1) <> (Three x2 y2 z2) = Three (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) =
    Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

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

type IdentityAssoc
   = Identity String -> Identity String -> Identity String -> Bool

type TwoString = Two String String

type TwoAssoc = TwoString -> TwoString -> TwoString -> Bool

type ThreeString = Three String String String

type ThreeAssoc = ThreeString -> ThreeString -> ThreeString -> Bool

type FourString = Four String String String String

type FourAssoc = FourString -> FourString -> FourString -> Bool

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
