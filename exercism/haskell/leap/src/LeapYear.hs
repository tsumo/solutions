module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year
    | rem400    = True
    | rem100    = False
    | rem4      = True
    | otherwise = False
    where rem4   = year `mod` 4   == 0
          rem100 = year `mod` 100 == 0
          rem400 = year `mod` 400 == 0

