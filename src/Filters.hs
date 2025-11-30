module Filters (shouldDelete) where

import Data.Char (isLetter, toLower)

isVowel :: Char -> Bool
isVowel c = toLower c `elem` "aeiouаеєиіїоуюя"

isConsonant :: Char -> Bool
isConsonant c = isLetter c && not (isVowel c)

lettersOnly :: String -> String
lettersOnly = filter isLetter

shouldDelete :: Int -> String -> Bool
shouldDelete n w =
    let clean = lettersOnly w
        l = length clean
    in  l == n
        && not (null clean)
        && isConsonant (head clean)
