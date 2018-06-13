module LearnParsers where

import           Data.Monoid
import Debug.Trace (trace)

import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

-- oneTwo'' = do
--     a <- char '1'
--     b <- char '2'
--     return (a : [b])
testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testStringParse :: Parser String -> IO ()
testStringParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

string' :: String -> Parser String
string' str = go str mempty
  where
    go (x:xs) parsed = char x >>= (\x' -> go xs (trace " " (parsed ++ [x'])))
    go [] parsed     = return parsed

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one stop"
  testParse one'
  pNL "oneTwo"
  testParse oneTwo
  pNL "oneTwo stop"
  testParse oneTwo'
