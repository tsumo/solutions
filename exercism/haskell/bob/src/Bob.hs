module Bob (responseFor) where
import Data.Char

responseFor :: String -> String
responseFor str
    | allCaps && lastChar == '?' = "Calm down, I know what I'm doing!"
    | allSilent = "Fine. Be that way!"
    | lastChar == '?' = "Sure."
    | allCaps = "Whoa, chill out!"
    | otherwise = "Whatever."
    where stripped = filter (\x -> not $ isSpace $ x) str
          allSilent = all isSpace str
          lastChar = last stripped
          alphaChars = filter isAlpha stripped
          allCaps = all isUpper alphaChars && alphaChars /= ""

