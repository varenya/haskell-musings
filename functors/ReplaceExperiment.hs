module ReplaceExperiment where

-- fmap :: (a -> b) -> f a -> f b
replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "wooho"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f b -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLiftedReplace :: (Functor f1,Functor f2) => f1 (f2 b) -> f1 (f2 Char)
twiceLiftedReplace = (fmap . fmap) replaceWithP

twiceLiftedReplace' :: [Maybe [Char]] -> [Maybe Char]
twiceLiftedReplace' =  twiceLiftedReplace

thriceLiftedReplace :: (Functor f1,Functor f2,Functor f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 Char))
thriceLiftedReplace = (fmap . fmap . fmap) replaceWithP

thriceLiftedReplace' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLiftedReplace' =  thriceLiftedReplace

main :: IO ()
main = do
  print lms
  putStr "replaceWithP' lms:   "
  print (replaceWithP' lms)
  putStr "liftedReplace lms:   "
  print (liftedReplace lms)
  putStr "liftedReplace' lms   "
  print (liftedReplace' lms)
  putStr "twiceliftedReplace lms:  "
  print (twiceLiftedReplace lms)
  putStr "twiceliftedReplace' lms:   "
  print (twiceLiftedReplace' lms)
  putStr "thriceliftedReplace lms:   "
  print (thriceLiftedReplace lms)
  putStr "thriceliftedReplace' lms:   "
  print (thriceLiftedReplace' lms)

