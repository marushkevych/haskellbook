module NumToWords where

  type RegisterName = Maybe String

  type Words = [String]

  data Register = Register Integer RegisterName deriving Show

  -- main :: IO()
  -- main = do
  --   putStr "Enter the number: "
  --   n <- getLine
  --   putStrLn (numToString $ read n)
  --   main

  numToString :: Integer -> String
  numToString 0 = ""
  numToString n =
    let tripples = reverse (toRegisters n []) in
      -- foldl f z [x1, x2, ..., xn]
      unwords $ foldl registerToWords [] tripples

  toRegisters :: Integer -> [Register] -> [Register]
  toRegisters n acc = 
    let 
      (rem, tripple) = divMod n 1000 
      reg = Register tripple (registerName (length acc))
    in
      if rem == 0 
        then reg : acc 
        else toRegisters rem (reg : acc)

  registerToWords :: Words -> Register -> Words
  registerToWords acc (Register 0 _) = acc
  registerToWords acc (Register tripple Nothing) = hundredsToWords tripple ++ acc
  registerToWords acc (Register tripple (Just register)) = 
    hundredsToWords tripple ++ register : acc

  hundredsToWords :: Integer -> Words
  hundredsToWords n = 
    let (hundrets, tens) = divMod n 100 in
      if hundrets == 0 
        then tensToWords tens
        else spellOnes hundrets : "hundred" : tensToWords tens

  tensToWords :: Integer -> Words
  tensToWords 0 = []
  tensToWords n = 
    if n < 9 then [spellOnes n]
    else
      if n < 20 then [spellTeens n]
      else
        let (tens, ones) = divMod n 10 in
          case ones of
            0 -> [spellTyes tens]
            _ -> [spellTyes tens, spellOnes ones]

  registerName :: Int -> RegisterName
  registerName 0 = Nothing
  registerName 1 = Just "thousand"
  registerName 2 = Just "million"
  registerName 3 = Just "billion"
  registerName _ = error "unsupported register - number is too large"

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
  spellOnes n = "cant spell" ++ show n


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
