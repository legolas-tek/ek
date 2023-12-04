{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- parser
-}

module Parser where

type ParserError = String

type Parser a = String -> Either ParserError (a, String)

parseChar :: Char -> Parser Char
parseChar expected [] = Left $ "Expected '" ++ [expected] ++ "' but found EOF"
parseChar expected (x:xs)
    | x == expected = Right (x, xs)
    | otherwise     = Left $ "Expected '" ++ [expected] ++ "' but found '" ++ [x] ++ "'"



