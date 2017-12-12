module NumToWords where

  main :: IO()
  main = do
    putStr "0 "
    print $ numToWords 0
    putStr "123 "
    print $ numToWords 123
    putStr "1,000,002 "
    print $ numToWords 1000002
    putStr "204,005,019,000 "
    print $ numToWords 204005019000
    putStr "204,000,003,000 "
    print $ numToWords 204000003000

  numToWords :: Integer -> String
  numToWords 0 = "zero"
  numToWords n =
    let tripples = reverse (toTripples n []) in
      -- foldl f z [x1, x2, ..., xn]
      unwords $ filter (not . null) $ foldl trippleToWords [] tripples

  toTripples :: Integer -> [Integer] -> [Integer]
  toTripples n acc = 
    let (rem, tripple) = divMod n 1000 in
      if rem == 0 
        then tripple : acc 
        else toTripples rem (tripple : acc)

  trippleToWords :: [String] -> Integer -> [String]
  trippleToWords acc 0 = "" : acc
  trippleToWords acc tripple = 
    let 
      registerString = register (length acc)
      tripleString = spellHundrets tripple
    in
      (tripleString ++ registerString) : acc

  register :: Int -> String
  register 0 = ""
  register 1 = " thousand"
  register 2 = " million"
  register 3 = " billion"
  register _ = error "unsupported register - number is too large"

  spellHundrets :: Integer -> String
  spellHundrets n = 
    let (hundrets, tens) = divMod n 100 in
      if hundrets == 0 
        then unwords $ spellTens tens
        else unwords $ spellOnes hundrets : "hundred" : spellTens tens

  spellTens :: Integer -> [String]
  spellTens 0 = []
  spellTens n = 
    if n < 9 then [spellOnes n]
    else
      if n < 20 then [spellTeens n]
      else
        let (tens, ones) = divMod n 10 in
            [spellTyes tens, spellOnes ones]


  spellOnes :: Integer -> String
  spellOnes 1 = "one"
  spellOnes 2 = "two"
  spellOnes 3 = "three"
  spellOnes 4 = "four"
  spellOnes 5 = "five"
  spellOnes 6 = "six"
  spellOnes 7 = "seven"
  spellOnes 8 = "eight"
  spellOnes 9 = "nine"

  spellTeens :: Integer -> String
  spellTeens 10 = "ten"
  spellTeens 11 = "eleven"
  spellTeens 12 = "twelve"
  spellTeens 13 = "thirteen"
  spellTeens 14 = "fourteen"
  spellTeens 15 = "fifteen"
  spellTeens 16 = "sixteen"
  spellTeens 17 = "seventeen"
  spellTeens 18 = "eighteen"
  spellTeens 19 = "nineteen"

  spellTyes :: Integer -> String
  spellTyes 2 = "twenty"
  spellTyes 3 = "thirty"
  spellTyes 4 = "fourty"
  spellTyes 5 = "fifty"
  spellTyes 6 = "sixty"
  spellTyes 7 = "seventy"
  spellTyes 8 = "eighty"
  spellTyes 9 = "nighty"
