module Tokenizer (tokenize, isWordChar) where

import Data.Char (isLetter)

isWordChar :: Char -> Bool
isWordChar c = isLetter c || c == '\'' || c == '-'

tokenize :: String -> [String]
tokenize [] = []
tokenize s@(c:cs)
    | isWordChar c =
        let (w, rest) = span isWordChar s
        in w : tokenize rest
    | c == ' ' || c == '\t' || c == '\n' =
        let (sp, rest) = span (\x -> x == ' ' || x == '\t' || x == '\n') s
        in sp : tokenize rest
    | otherwise =
        [ [c] ] ++ tokenize cs
