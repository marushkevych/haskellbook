module RegisteredUser where
  newtype Username = Username String

  newtype AccountNumber = AccountNumber Integer

  data User = 
      UnregisteredUser
    | RegisteredUser Username AccountNumber

  user = RegisteredUser (Username "John") (AccountNumber 123)

  printUser :: User -> IO ()
  printUser UnregisteredUser = putStrLn "unregistered user"
  printUser (RegisteredUser (Username name) (AccountNumber number)) =
    putStrLn $ "Name " ++ name ++ " Account " ++ (show number)