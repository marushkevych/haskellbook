add :: String -> String -> String
add a b = a ++ b

getChar1 :: String -> Int -> Char
getChar1 s i = s !! i

drop1 :: String -> Int -> String
drop1 s i = drop i s

thirdLetter :: String -> Char
thirdLetter x = (!!) x 2

getFromString :: Int -> Char
getFromString = (!!) str

str = "Curry is awesome"

rvrs = 
  let first = take 5 str
      remainderAferFirst = drop 6 str
      second = take 2 remainderAferFirst
      third = drop 3 remainderAferFirst

  in third ++ " " ++ second ++ " " ++ first
