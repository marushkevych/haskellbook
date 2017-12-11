
module DetermineTheType where 

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = 
  if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

i :: a -> a
i x = x 

c :: b -> a -> b
c x _ = x

c'' :: b -> a -> b
c'' = c

c' :: a -> b -> b
c' _ y = y

r :: [a] -> [a]
r xs = drop 2 xs

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' xToy x = xToy x


