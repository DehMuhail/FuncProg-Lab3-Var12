module Main where

import System.IO
import Tokenizer
import Filters
import Cleaner

processTokens :: Int -> [String] -> [String]
processTokens n = map apply
  where
    apply t
        | all isWordChar t =
            if shouldDelete n t then "" else t
        | otherwise = t

rebuild :: [String] -> String
rebuild = concat

main :: IO ()
main = do
    putStr "Введіть довжину слова для видалення: "
    hFlush stdout
    n <- readLn

    content <- readFile "input.txt"

    let tokens    = tokenize content
        processed = processTokens n tokens
        raw       = rebuild processed
        cleaned   = cleanSpaces raw

    writeFile "output.txt" cleaned
    putStrLn "\nГотово! Результат записано у output.txt"
