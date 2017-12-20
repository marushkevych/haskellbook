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
  numToString 0 = "Zero"
  numToString n =
    let registers = reverse (toRegisters n []) in
      -- foldl f z [x1, x2, ..., xn]
      unwords $ foldl registerToWords [] registers

  toRegisters :: Integer -> [Register] -> [Register]
  toRegisters n acc = 
    let 
      (thousands, tripple) = divMod n 1000 
      reg = Register tripple (registerName (length acc))
    in
      case thousands of
        0 -> reg : acc
        t -> toRegisters t (reg : acc)

  registerToWords :: Words -> Register -> Words
  registerToWords acc (Register 0 _) = acc
  registerToWords acc (Register tripple Nothing) = trippleToWords tripple ++ acc
  registerToWords acc (Register tripple (Just register)) = 
    trippleToWords tripple ++ register : acc

  trippleToWords :: Integer -> Words
  trippleToWords n = 
    let (hundrets, tens) = divMod n 100 in
      hundretsToWords hundrets ++ tensToWords tens

  tensToWords :: Integer -> Words
  tensToWords 0 = []
  tensToWords n = 
    if n < 20 then [spellOnes n]
    else
      let (tens, ones) = divMod n 10 in
        spellTyes tens : onesToWords ones

  hundretsToWords :: Integer -> Words
  hundretsToWords 0 = []
  hundretsToWords n = [spellOnes n, "hundred"]

  onesToWords :: Integer -> Words
  onesToWords 0 = []
  onesToWords n = [spellOnes n]

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
  spellOnes 10 = "ten"
  spellOnes 11 = "eleven"
  spellOnes 12 = "twelve"
  spellOnes 13 = "thirteen"
  spellOnes 14 = "fourteen"
  spellOnes 15 = "fifteen"
  spellOnes 16 = "sixteen"
  spellOnes 17 = "seventeen"
  spellOnes 18 = "eighteen"
  spellOnes 19 = "nineteen"

  spellTyes :: Integer -> String
  spellTyes 2 = "twenty"
  spellTyes 3 = "thirty"
  spellTyes 4 = "fourty"
  spellTyes 5 = "fifty"
  spellTyes 6 = "sixty"
  spellTyes 7 = "seventy"
  spellTyes 8 = "eighty"
  spellTyes 9 = "nighty"
