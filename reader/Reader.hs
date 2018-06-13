{-# LANGUAGE InstanceSigs #-}

import           Control.Applicative
import           Data.Char

boop = (* 2)

doop = (+ 10)

data Reader r a = Reader
  { runReader :: r -> a
  }

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

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

monadicTupled :: [Char] -> ([Char], [Char])
monadicTupled = do
  a <- cap
  b <- rev
  return (a, b)

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader (const a)
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader {runReader = f}) <*> (Reader {runReader = g}) =
    Reader {runReader = (\x -> f x (g x))}

instance Monad (Reader r) where
    return :: a -> Reader r a
    return = pure
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    Reader f >>= g  = Reader (\x -> monadicFun x x)
                where
                    monadicFun = runReader . g . f





foo :: (Functor f , Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t,Int)
bar t r  = (t , length r)

froot :: Num a => [a] -> ([a],Int)
froot r = bar (foo r) r





