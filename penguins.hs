data WherePenguinsLive = 
    Galapagos
    | Antartica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Show, Eq)

data Penguin = 
    Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antartaicPenguin :: Penguin -> Bool
antartaicPenguin (Peng Antartica) = True
antartaicPenguin _ = False

antarticaOrGalaPagos :: Penguin -> Bool
antarticaOrGalaPagos p = (galapagosPenguin p) || (antartaicPenguin p)



