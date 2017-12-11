module Date where

  data DayOfWeek = 
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    deriving (Ord, Show)

  instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

  data Date = Date DayOfWeek Int deriving Show

  instance Eq Date where
    (==) (Date dayOfWeek day) (Date dayOfWeek' day') =
      dayOfWeek == dayOfWeek' && day == day'

  today = Date Mon 10
  tomorrow = Date Tue 11
  someDay = Date Mon 10