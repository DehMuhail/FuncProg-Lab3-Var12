module Cleaner (cleanSpaces) where

import Data.Char (isSpace)

cleanSpaces :: String -> String
cleanSpaces = unlines . map cleanLine . lines
  where
    cleanLine :: String -> String
    cleanLine = trim . fixPunctuation . collapseSpaces

    collapseSpaces :: String -> String
    collapseSpaces [] = []
    collapseSpaces (c:cs)
        | c == ' ' || c == '\t' =
            ' ' : collapseSpaces (dropWhile (\x -> x == ' ' || x == '\t') cs)
        | otherwise = c : collapseSpaces cs

    fixPunctuation :: String -> String
    fixPunctuation [] = []
    fixPunctuation (c:cs)
        | c == ' ' && not (null cs) && head cs `elem` ",.!?;:')\"]" =
            fixPunctuation cs
        | otherwise =
            c : fixPunctuation cs

    trim :: String -> String
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
