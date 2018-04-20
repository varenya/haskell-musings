module WordNumberTest where

import           Data.Char
import           Data.List       (sort)
import           Recursion       (digitToWord, digits, wordNumber)
import           Test.Hspec
import           Test.QuickCheck

twice f = f . f

fourTimes = twice . twice

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (* 2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t)      = (Just y, t)
    go y (Just x, _)       = (Just y, x >= y)

main :: IO ()
main =
  hspec $ do
    describe "digitToWord" $ do
      it "returns zero for 0" $ do digitToWord 0 `shouldBe` "zero"
      it "returns one for 1" $ do digitToWord 1 `shouldBe` "one"
    describe "digts" $ do
      it "returns [1] for 1" $ do digits 1 `shouldBe` [1]
      it "returns [1,0,0] for 100" $ do digits 100 `shouldBe` [1, 0, 0]
    describe "wordNumber" $ do
      it "one-zero-zero given 100" $ do
        wordNumber 100 `shouldBe` "one-zero-zero"
      it "nine-zero-zero-one for 9001" $ do
        wordNumber 9001 `shouldBe` "nine-zero-zero-one"
    describe "half" $ do
      it "should return the x when multiplied by 2" $ do
        property $ \x -> halfIdentity x == (x :: Float)
    describe "double reverse gives the same list" $ do
      it "should return the x when double reversed" $ do
        property $ \x -> (reverse . reverse) x == (x :: [Float])
    describe "sort" $ do
      it "should return True for the listOrdered property" $ do
        property $ \x -> (listOrdered . sort) (x :: [Int]) == True

plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative x y = x + y == y + x

multiplyAssociative x y z = x * (y * z) == (x * y) * z

multiplyCommutative x y = x * y == y * x

twoInts :: Gen (Int, Int)
twoInts = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

threeInts :: Gen (Int, Int, Int)
threeInts = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

prop_plussAssociative = forAll threeInts (\(x, y, z) -> plusAssociative x y z)

prop_plusCommutative = forAll twoInts (\(x, y) -> plusCommutative x y)

prop_multiplyAssociative =
  forAll threeInts (\(x, y, z) -> multiplyAssociative x y z)

prop_multiplyCommutative = forAll twoInts (\(x, y) -> multiplyCommutative x y)

quotientProperty x y = (quot x y) * y + (rem x y) == x

reminderProperty x y = (div x y) * y + (mod x y) == x

propQuotient = forAll twoInts (\(x, y) -> quotientProperty x y)

propReminder = forAll twoInts (\(x, y) -> reminderProperty x y)

propDollar = forAll (arbitrary :: Gen Int) (\x -> id . id $ x == (id (id x)))

square x = x * x

add2 = (+ 2)

compose f g = (\x -> f (g x))

propCompose =
  forAll
    (arbitrary :: Gen Int)
    (\x -> (square . add2) x == (compose square add2) x)

joinList = forAll genIntList (\(x, y) -> (foldr (:) y x) == (x ++ y))

genIntList :: Gen ([Int], [Int])
genIntList = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

-- nonNegativeArbitrary = arbitrary `suchThat` (> 0)
genNumList :: Gen ([Int], Int)
genNumList = do
  a <- arbitrary
  b <- arbitrary `suchThat` (\x -> (x <= length a) && (x > 0))
  return (a, b)

customConcat = foldr (++) []

concatList =
  forAll (arbitrary :: Gen [[Int]]) (\x -> concat x == customConcat x)

lengthCheck = forAll genNumList (\(xs, n) -> (length (take n xs)) == n)

capitalizeWord :: String -> String
capitalizeWord = map toUpper

checkCapitalizeWord :: Property
checkCapitalizeWord =
  forAll
    (arbitrary :: Gen String)
    (\str -> capitalizeWord str == (fourTimes capitalizeWord str))

checkSort :: Property 
checkSort = 
  forAll (arbitrary :: Gen [Int]) (\arr -> sort arr == (fourTimes sort arr))

runTest :: IO ()
runTest = do
  putStrLn "plussAssociative"
  quickCheck prop_plussAssociative
  putStrLn "plusCommutative"
  quickCheck prop_plusCommutative
  putStrLn "multiply Associative"
  quickCheck prop_multiplyAssociative
  putStrLn "multiply Commutative"
  quickCheck prop_multiplyCommutative
  putStrLn "quotient property"
  quickCheck propQuotient
  putStrLn "reminder property"
  quickCheck propReminder
  putStrLn "dollar operator"
  quickCheck propDollar
  putStrLn "compose operator"
  quickCheck propCompose
  putStrLn "joinList operator"
  quickCheck joinList
  putStrLn "concatList operator"
  quickCheck concatList
  -- putStrLn "check length and take"
  -- quickCheck lengthCheck
  putStrLn "capitalize word idemponent"
  quickCheck checkCapitalizeWord
  putStrLn "check sort idempotency"
  quickCheck checkSort
