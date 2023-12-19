{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- lisp parsing
-}

module Lisp (parseSExpr, comment, lineComment) where

import Parser
import SExpr

integerLit :: Parser SExpr
integerLit = IntegerLit <$> parseInt

symbolRef :: Parser String
symbolRef = some $ parseAnyChar $ ['A'..'Z'] ++ ['a'..'z'] ++ "+-*/%$#?"

symbol :: Parser SExpr
symbol = Symbol <$> symbolRef

list :: Parser SExpr
list = List <$> parseList parseSExpr

string :: Parser SExpr
string = StringLit <$> parseString

parseSExpr :: Parser SExpr
parseSExpr = useless >> integerLit <|> symbol <|> list <|> string

useless :: Parser [String]
useless = many (((\x -> [x]) <$> parseChar ' ') <|> comment)

lineComment :: Parser String
lineComment = parseChar ';' >> many (parseAnyButChar '\n') >> return ""

comment :: Parser String
comment = parseChar '#' >> parseChar '|' >> many (parseAnyButChar '|')
    >> parseChar '|' >> parseChar '#' >> return ""
