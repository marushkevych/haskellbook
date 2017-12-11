module Reverse where

rvrs :: String -> String
rvrs str = 
  let first = take 5 str
      remainderAferFirst = drop 6 str
      second = take 2 remainderAferFirst
      third = drop 3 remainderAferFirst

  in third ++ " " ++ second ++ " " ++ first


main :: IO ()
main = print $ rvrs "Curry is awesome"