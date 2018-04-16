data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs x = if  x < 0 then
                (negate x)
          else
                x
firstItem :: [a] -> a
firstItem (x:xs)  = x

firstTuple :: (a,b) -> a
firstTuple (a,b) = a

h:: (Num a,Num b) => a -> b -> b 
h = undefined