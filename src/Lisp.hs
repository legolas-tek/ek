{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- lisp parsing
-}

module Lisp (parseSExpr) where

import Parser
import SExpr

integerLit :: Parser SExpr
integerLit = IntegerLit <$> parseInt

symbolRef :: Parser String
symbolRef = some $ parseAnyChar $ ['A'..'Z'] ++ ['a'..'z'] ++ "+-/%$#"

symbol :: Parser SExpr
symbol = Symbol <$> symbolRef

list :: Parser SExpr
list = List <$> parseList parseSExpr

parseSExpr :: Parser SExpr
parseSExpr = spaces >> integerLit <|> symbol <|> list
