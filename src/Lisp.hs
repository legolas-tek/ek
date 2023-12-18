{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- lisp parsing
-}

module Lisp (parseSExpr, parseComment, parseCommentLine) where

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
parseSExpr = parseComment <|> spaces >> integerLit <|> symbol <|> list <|> string

parseCommentLine :: Parser String
parseCommentLine = spaces >> parseChar ';' >> many (parseAnyButChar '\0') >> return ""

parseComment :: Parser String
parseComment = parseChar '#' >> parseChar '|' >> many (parseAnyButChar '|')
    >> parseChar '|' >> parseChar '#' >> return ""
