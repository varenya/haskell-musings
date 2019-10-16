
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

getPrice :: Vehicle -> Price
getPrice (Car _ price) = price
getPrice (Plane _) = (Price 0)

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
