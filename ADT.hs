data Price = Price Integer deriving (Eq,Show)

data Manufacturer = 
            Mini
        |   Mazda
        |   Tata
        deriving (Eq, Show)

data Airline = 
      PapuAir
    | CatapultsR'Us
    | TakeUrChanceUnited deriving (Eq,Show)

data Vehicle =
    Car Manufacturer Price | Plane Airline deriving (Eq,Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 10000)
clownCar = Car Tata (Price 100)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane   _       = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car man _) = man

data GuessWhat =
    ChickenButt deriving (Eq,Show)

data Id a = 
    MkId a deriving (Eq,Show)

data Product a b =
    Product a b deriving (Eq,Show)

data Sum a b =
    First a | Second b deriving (Eq,Show)

data RecordProduct a b = 
    RecordProduct { pfirst :: a , psecond :: b} deriving (Eq,Show)

newtype NumCow = 
    NumCow Int deriving (Eq,Show)
newtype NumPig = 
    NumPig Int deriving (Eq,Show)

data FarmHouse =
    FarmHouse NumCow NumPig deriving (Eq,Show)

type FarmHouse' = Product NumCow NumPig



newtype NumSheep =
    NumSheep Int deriving (Eq,Show)

data BigFarmHouse =
    BigFarmHouse NumCow NumPig NumSheep deriving (Eq,Show)

type BigFarmHouse' = Product NumCow (Product NumPig NumSheep)

type PoundsOfWool = Int
type Age = Int
type LovesMud = Bool
type Name = String

data CowInfo =
    CowInfo Name Age deriving (Eq,Show)

data PigInfo = 
    PigInfo Name Age LovesMud deriving (Eq,Show)

data SheepInfo =
    SheepInfo Name Age PoundsOfWool deriving (Eq,Show)

data Animal = 
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo

  deriving (Eq,Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

idIdentity :: Id (a -> a)
idIdentity = MkId $ \x -> x

doNothing :: Id a -> a
doNothing (MkId f) = f

data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
    , lang :: ProgrammingLanguage }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
        [ GnuPlusLinux
        , OpenBSDPlusNevermindJustBSDStill
        , Mac
        , Windows
        ]
allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer x y | x <- allOperatingSystems ,y <- allLanguages]

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer 
                | WheatFarmer
                | SoybeanFarmer deriving Show

data Farmer =
    Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _                        = False

data FarmerRec =
    FarmerRect { name :: Name , acres :: Acres , farmerType :: FarmerType } deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = 
    case farmerType farmer of 
            DairyFarmer -> True
            _           -> False
