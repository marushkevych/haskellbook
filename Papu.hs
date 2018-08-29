module Papu where
  import Data.List
  
  data Rocks = Rocks String deriving (Eq, Show)

  data Yeah = Yeah Bool deriving (Eq, Show)

  data Papu = Papu Rocks Yeah deriving (Eq, Show)

  truth :: Papu
  truth = Papu (Rocks "chomskydoz") (Yeah True)

  equalityForall :: Papu -> Papu -> Bool
  equalityForall p p' = p == p'

  f :: RealFrac a => a
  f = 1.0

  freud :: Int -> Int
  freud x = x

  myX = 1 :: Int 

  sigmund :: Num a => a -> a
  sigmund x = x * 3

  jung :: Ord a => [a] -> a
  jung xs = head (sort xs)


  mySort :: Ord a => [a] -> [a]
  mySort = sort

  -- signifier :: [Char] -> Char
  signifier :: Ord a => [a] -> a
  signifier xs = head (mySort xs)

