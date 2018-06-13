module Moi where

import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.DList                as DL

newtype Moi s a = Moi
  { runMoi :: s -> (a, s)
  }

getMoi :: Moi s s
getMoi = Moi {runMoi = \s -> (s, s)}

putMoi :: s -> Moi s ()
putMoi s = Moi {runMoi = \_ -> ((), s)}

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd (sa s)

eval :: Moi s a -> s -> a
eval (Moi f) s = fst (f s)

modifyMoi :: (s -> s) -> Moi s ()
modifyMoi f = Moi (\s -> ((), f s))

stateFunctor :: (a -> b) -> (s -> (a, s)) -> s -> (b, s)
stateFunctor f g x =
  let (aVal, sValue) = g x
  in (f aVal, sValue)

instance Functor (Moi s) where
  fmap f (Moi g) = Moi (stateFunctor f g)

stateApplicative :: (s -> (a -> b, s)) -> (s -> (a, s)) -> s -> (b, s)
stateApplicative f g x =
  let (transform, s1) = f x
      (aVal, sValue) = g s1
  in (transform aVal, sValue)

instance Applicative (Moi s) where
  pure a = Moi {runMoi = (\s -> (a, s))}
  Moi f <*> Moi g = Moi (stateApplicative f g)

stateMonad :: (s -> (a, s)) -> (a -> Moi s b) -> s -> (b, s)
stateMonad f g x =
  let (aValue, sValue) = f x
      stateFun = runMoi $ g aValue
  in stateFun x

instance Monad (Moi s) where
  return = pure
  (Moi f) >>= g = Moi (stateMonad f g)

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Fizz"
  | n `mod` 3 == 0 = "Buzz"
  | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  let dList = execState (mapM_ addResult list) DL.empty
  in DL.apply dList []

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzList [1 .. 100]
