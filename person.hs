type Name = String

type Age = Integer

data Person =
  Person Name
         Age
  deriving (Eq, Show)

data PersonInValid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInValid Person
mkPerson name age
  | name /= "" && age > 0 = Right (Person name age)
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise =
    Left $
    PersonInvalidUnknown $ "Name was " ++ show name ++ " Age was " ++ show age

gimmePerson :: IO ()
gimmePerson = 
    do
        putStr "Enter your name :"
        name <- getLine
        putStr "Enter your age :"
        stringAge  <- getLine 
        let age = (read stringAge) :: Age
        case (mkPerson name age) of
            Left NameEmpty -> putStrLn "Name cannot be empty!"
            Left AgeTooLow -> putStrLn "Age cannot be negative!"
            Left (PersonInvalidUnknown error) -> putStrLn error
            Right (Person name age)  -> putStrLn "Yay! SuccessFully created"
