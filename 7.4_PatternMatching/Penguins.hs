module Penguins where

  data WherePenguinsLive = 
      Galapagos
    | Antarctica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)

  data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

  galapagosPenguin :: Penguin -> Bool 
  galapagosPenguin (Peng Galapagos) = True 
  galapagosPenguin _ = False

  antarcticPenguin :: Penguin -> Bool 
  antarcticPenguin (Peng Antarctica) = True 
  antarcticPenguin _ = False

  antarcticOrGalapagos :: Penguin -> Bool 
  antarcticOrGalapagos p = (galapagosPenguin p) || (antarcticPenguin p)