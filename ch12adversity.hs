import           Data.List

notThe :: String -> Maybe String
notThe str
  | "the" `isInfixOf` str = Just str
  | otherwise = Nothing

putThe :: String -> String
putThe "the" = "a"
putThe str   = str

replaceThe :: String -> String
replaceThe [] = []
replaceThe str =
  case (notThe str) of
    Just str -> (unwords $ map putThe $ words str)
    Nothing  -> str

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = countHelper (words str)
  where
    vowels = "aeiou"
    countHelper [] = 0
    countHelper (w:[]) = 0
    countHelper (w1:w2:ws)
      | w1 == "the" && (head w2 `elem` vowels) = 1 + countHelper ws
      | otherwise = countHelper ws

countVowels :: String -> Integer
countVowels "" = 0
countVowels (c:cs)
  | isVowel c = 1 + countVowels cs
  | otherwise = countVowels cs
  where
    isVowel c = elem c "aeiou"

countConsonants :: String -> Integer
countConsonants ""  = 0
countConsonants str = (toInteger (length str) - countVowels str)

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord word
  | countVowels word > countConsonants word = Nothing
  | otherwise = Just (Word' word)

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero        = 0
natToInteger (Succ nats) = 1 + natToInteger nats

integerToNat :: Integer -> Maybe Nat
integerToNat input
  | input < 0 = Nothing
  | otherwise = Just (int2NatHelper input)
  where
    int2NatHelper 0     = Zero
    int2NatHelper input = (Succ $ int2NatHelper (input - 1))

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe defValue _ Nothing                = defValue
mayybe defValue applicateFunc (Just val) = applicateFunc val

fromMaybe :: a -> Maybe a -> a
fromMaybe defValue = mayybe defValue id

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (w:ws) = Just w

maybeToList :: Maybe a -> [a]
maybeToList Nothing     = []
maybeToList (Just list) = [list]

catMaybes :: [Maybe a] -> [a]
catMaybes []                  = []
catMaybes (Nothing:items)     = catMaybes items
catMaybes ((Just item):items) = item : catMaybes items

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe list
  | (any id $ map isNothing list) = Nothing
  | otherwise = Just (catMaybes list)

foldLeft :: Either a b -> [a] -> [a]
foldLeft (Left a) accum  = a : accum
foldLeft (Right a) accum = accum

foldRight :: Either a b -> [b] -> [b]
foldRight (Right current) accum = current : accum
foldRight (Left current) accum  = accum

partitionEither :: Either a b -> ([a], [b]) -> ([a], [b])
partitionEither (Left curr) (l, r)  = (curr : l, r)
partitionEither (Right curr) (l, r) = (l, curr : r)

lefts' :: [Either a b] -> [a]
lefts' = foldr foldLeft []

rights' :: [Either a b] -> [b]
rights' = foldr foldRight []

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr partitionEither ([], [])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' fmapper (Left val)  = Nothing
eitherMaybe' fmapper (Right val) = Just (fmapper val)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' leftMap _ (Left val)   = leftMap val
either' _ rightMap (Right val) = rightMap val

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' rightMap = either' (\_ -> Nothing) (\val -> Just (rightMap val))

myIterate :: (a -> a) -> a -> [a]
myIterate fmapper init = unfoldr (\b -> Just (b, fmapper b)) init

myUnFoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnFoldr f z =
  case (f z) of
    Just (curr, nextValue) -> curr : myUnFoldr f nextValue
    Nothing                -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate fmapper init = myUnFoldr (\b -> Just (b, fmapper b)) init

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold fmapper initial =
  case (fmapper initial) of
    Nothing        -> Leaf
    Just (x, y, z) -> Node (unfold fmapper x) y (unfold fmapper z)

dec x = (x - 1)

inc = (+ 1)

treeGenerator :: Integer -> Integer -> Maybe (Integer, Integer, Integer)
treeGenerator stopValue startValue
  | stopValue == startValue = Nothing
  | otherwise = Just (inc startValue, startValue, inc startValue)

treeBuild :: Integer -> BinaryTree Integer
treeBuild inputNum = unfold (treeGenerator inputNum) 0
