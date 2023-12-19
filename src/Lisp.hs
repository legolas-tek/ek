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
symbolRef = some $ parseAnyChar $ ['A'..'Z'] ++ ['a'..'z'] ++ "+-*/%$#?=<>"

symbol :: Parser SExpr
symbol = Symbol <$> symbolRef

list :: Parser SExpr
list = List <$> parseList parseSExpr

string :: Parser SExpr
string = StringLit <$> parseString

parseSExpr :: Parser SExpr
parseSExpr = useless >> integerLit <|> symbol <|> list <|> string

useless :: Parser [String]
useless = many ((parseAnyChar " \n\t" >> return "") <|> comment <|> lineComment)

lineComment :: Parser String
lineComment = parseChar ';' >> many (parseAnyButChar '\n') >> return ""

comment :: Parser String
comment = commentStart >> many (commentContent) >> commentEnd >> return ""

commentStart :: Parser Char
commentStart = parseChar '#' >> parseChar '|'

commentContent :: Parser String
commentContent = comment <|> nonComment
  where nonComment = parseNot commentEnd >> parseAny >> return ""

commentEnd :: Parser Char
commentEnd = parseChar '|' >> parseChar '#'
