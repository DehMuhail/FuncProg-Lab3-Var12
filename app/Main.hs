module Main where

import Data.Char (isLetter, toLower, isSpace)
import System.IO


type Symbol = Char
type TxtWord = String
type Sentence = [TxtWord]

isVowel :: Char -> Bool
isVowel c = toLower c `elem` "aeiouаеєиіїоуюя"

isConsonant :: Char -> Bool
isConsonant c = isLetter c && not (isVowel c)

shouldDelete :: Int -> TxtWord -> Bool
shouldDelete n w =
    let letters = filter isLetter w
        len = length letters
    in len == n
        && not (null letters)
        && isConsonant (head letters)


normalizeSpaces :: String -> String
normalizeSpaces = unwords . words . map (\c -> if isSpace c then ' ' else c)

processLine :: Int -> String -> String
processLine n line =
    let parts = splitPreservingPunctuation line
        kept  = [ p | p <- parts, not (shouldDelete n p) ]
    in joinTokens kept

splitPreservingPunctuation :: String -> [String]
splitPreservingPunctuation [] = []
splitPreservingPunctuation (c:cs)
    | isLetter c =
        let (letters, rest) = span isLetter cs
        in (c:letters) : splitPreservingPunctuation rest

    | isPunctuationChar c =
        [ [c] ] ++ splitPreservingPunctuation cs

    | isSpace c =
        splitPreservingPunctuation cs

    | otherwise =
        [ [c] ] ++ splitPreservingPunctuation cs

isPunctuationChar :: Char -> Bool
isPunctuationChar c = c `elem` ".,?!:;\"'()[]{}-–…"


joinTokens :: [String] -> String
joinTokens = go True
  where
    go _ [] = ""
    go isStart (t:ts)
        | null t = go isStart ts
        | isPunctuationToken t =
            t ++ go False ts
        | isStart =
            t ++ go False ts
        | otherwise =
            " " ++ t ++ go False ts

isPunctuationToken :: String -> Bool
isPunctuationToken [c] = isPunctuationChar c
isPunctuationToken _   = False

main :: IO ()
main = do
    putStr "Введіть довжину слова для видалення: "
    hFlush stdout
    n <- readLn

    content <- readFile "input.txt"
    let result = unlines $ map (processLine n) (lines content)

    writeFile "output.txt" result
    putStrLn "\nГотово! Результат записано у output.txt"
