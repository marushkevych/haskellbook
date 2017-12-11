module Learn where
sayHello :: String -> IO()
sayHello s = putStrLn ("Hello, " ++ s ++ "!")

multiply a b = a * b
byPi = multiply p
  where p = pi


f = z / x + y
  where x = 7
        y = negate x
        z = y * 10

waxOn = x * 5
  where
    z = 7
    x = y ^ 2
    y = z + 8

triple x = x * 3

waxOff x = triple x