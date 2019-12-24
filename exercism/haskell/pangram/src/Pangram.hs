module Pangram (isPangram) where
import Data.Char

isPangram :: String -> Bool
isPangram text = all inText ['a'..'z']
                 where textL = map toLower text
                       inText c = elem c textL

