module PenguinsLive where

data WherePenguinsLive =
      Galapagos
    | Antartica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAmerica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humbold  = Peng SouthAmerica
gentoo   = Peng Antartica
macaroni = Peng Antartica
little   = Peng Australia
gala     = Peng Galapagos

-- more pattern matching examples
galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarticPenguin :: Penguin -> Bool
antarticPenguin (Peng Antartica) = True
antarticPenguin _                = False

antarticOrGalapagos :: Penguin -> Bool
antarticOrGalapagos p = (galapagosPenguin p) || (antarticPenguin p)

