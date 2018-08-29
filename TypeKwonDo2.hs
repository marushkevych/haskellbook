module TypeKwonDo2 where

  chk :: Eq b => (a -> b) -> a -> b -> Bool
  chk f x y = f x == y

  arith :: Num b => (a -> b) -> Integer -> a -> b
  arith f n a =  (f a) + (fromInteger n)

  double x = 2 * x