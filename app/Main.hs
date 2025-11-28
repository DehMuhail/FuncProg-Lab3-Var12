module Main where

import Data.Char (isLetter, toLower, isSpace)
import System.IO

type TxtWord = String

-- Ukrainian + English vowels
isVowel :: Char -> Bool
isVowel c = toLower c `elem`
    "aeiouаеєиіїоуюя"

-- Consonant = letter that is not vowel
isConsonant :: Char -> Bool
isConsonant c = isLetter c && not (isVowel c)

-- Check if a character is part of a complex word
-- Letters, apostrophes, hyphens count as "in-word"
isWordChar :: Char -> Bool
isWordChar c = isLetter c || c == '\'' || c == '-'

-- Extract only letters for length logic
lettersOnly :: String -> String
lettersOnly = filter isLetter

-- Determine if a word should be deleted
shouldDelete :: Int -> String -> Bool
shouldDelete n w =
    let clean = lettersOnly w
        len   = length clean
    in  len == n
        && not (null clean)
        && isConsonant (head clean)

-- Tokenize text into:
-- [word-with-hyphens/apostrophes, punctuation, spaces]
tokenize :: String -> [String]
tokenize [] = []
tokenize s@(c:cs)
    | isWordChar c =
        let (w, rest) = span isWordChar s
        in w : tokenize rest

    | isSpace c =
        let (sp, rest) = span isSpace s
        in sp : tokenize rest

    | otherwise =
        [ [c] ] ++ tokenize cs

-- Process tokens, removing complex words when needed
processTokens :: Int -> [String] -> [String]
processTokens n = map remove
  where
    remove t
        | all isSpace t = t
        | all isWordChar t =
            if shouldDelete n t then "" else t
        | otherwise = t

-- Rebuild the full text as-is
rebuild :: [String] -> String
rebuild = concat

main :: IO ()
main = do
    putStr "Введіть довжину слова для видалення: "
    hFlush stdout
    n <- readLn

    content <- readFile "input.txt"

    let tokens = tokenize content
        processed = processTokens n tokens
        result = rebuild processed

    writeFile "output.txt" result
    putStrLn "\nГотово! Результат записано у output.txt"
