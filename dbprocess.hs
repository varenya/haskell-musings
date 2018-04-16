import Data.Time

data DataBaseItem = DbString String 
                  | DbNumber Integer 
                  | DbDate UTCTime
                   deriving (Eq,Ord,Show)

theDatabase :: [DataBaseItem]
theDatabase = 
        [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
                , DbNumber 9001
                , DbNumber 9004
                , DbString "Hello, world!"
                , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
                , DbNumber 9006
        ]
anotherDataBase:: [DataBaseItem]
anotherDataBase = 
        [
           DbNumber 9001
         , DbNumber 9004
         , DbString "Hello, world!"
        ]
early :: UTCTime
early = UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0)

getUTCTime :: DataBaseItem -> [UTCTime] -> [UTCTime]
getUTCTime (DbDate t) accum  = t : accum
getUTCTime _ accum = accum

getDbNumber :: DataBaseItem -> [Integer] -> [Integer]
getDbNumber (DbNumber number) accum = number : accum
getDbNumber _ accum = accum


filterDbDate :: [DataBaseItem] -> [UTCTime]
filterDbDate = foldr getUTCTime []  

filterDbNumber :: [DataBaseItem] -> [Integer]
filterDbNumber = foldr getDbNumber []

mostRecent :: [DataBaseItem] -> UTCTime
mostRecent = (foldr max early . filterDbDate)

sumDb :: [DataBaseItem] -> Integer
sumDb = (foldr (+) 0 . filterDbNumber)

avgDb :: [DataBaseItem] -> Double
avgDb db = (fromIntegral sumOfdb)/(fromIntegral count) 
                where
                        sumOfdb = sumDb db
                        count = length $ filterDbNumber db

factorial = 1 : scanl (*) 2 factorial
