import           Data.Monoid
import           Test.QuickCheck

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

genOnly :: Arbitrary a => Gen (Optional a)
genOnly = do
  a <- arbitrary
  return (Only a)

genOptional :: (Arbitrary a) => Gen (Optional a)
genOptional = frequency [(1, return Nada), (10, genOnly)]

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = genOptional

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend x Nada            = x
  mappend Nada y            = y
  mappend (Only x) (Only y) = Only (mappend x y)

newtype First' a = First'
  { getFirst' :: Optional a
  } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' {getFirst' = Nada}
  mappend x (First' {getFirst' = Nada}) = x
  mappend (First' {getFirst' = Nada}) y = y
  mappend (First' {getFirst' = x}) (First' {getFirst' = _}) =
    First' {getFirst' = x}

genFirst :: Arbitrary a => Gen (First' a)
genFirst = do
  a <- arbitrary
  return (First' a)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity m = (mempty <> m) == m

monoidRightIdenity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdenity m = (m <> mempty) == m

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdenity :: FstId)
