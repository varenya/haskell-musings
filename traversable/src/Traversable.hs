import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a)
         a
         (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node leftTree leaf rightTree) =
    Node (fmap f leftTree) (f leaf) (fmap f rightTree)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node leftTree x rightTree) =
    (foldMap f leftTree) <> (f x) <> (foldMap f rightTree)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> (f x)
  traverse f (Node leftTree x rightTree) =
    Node <$> (traverse f leftTree) <*> (f x) <*> (traverse f rightTree)

treeGen :: Arbitrary a => Gen (Tree a)
treeGen = do
  xs <- treeGen
  ys <- treeGen
  a <- arbitrary
  elements [Leaf a, Empty, Node xs a ys]

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = treeGen

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

type Trigger = (Int, Int, [Int])

main :: IO ()
main = do
  let trigger = undefined
  quickBatch $ traversable (trigger :: Tree Trigger)
