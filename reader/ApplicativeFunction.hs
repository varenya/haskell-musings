newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName   :: DogName
  , address   :: Address
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName    :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "varenya") (DogName "luffy") (Address "sesame street")

chris :: Person
chris = Person (HumanName "Chris") (DogName "papu") (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    address <- address
    return $ Dog name address

instance Monad (Reader r) where
