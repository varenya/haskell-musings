import Control.Applicative
import Data.Char

boop = (*2)

doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop


cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char],[Char])
tupled = (,) <$> cap <*> rev

monadicTupled :: [Char] -> ([Char],[Char])
monadicTupled = do
        a <- cap
        b <- rev
        return (a,b)

