{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- lisp parsing
-}

module Lisp (parseSExpr) where

import Parser
import SExpr

integerLit :: Parser Char SExpr
integerLit = IntegerLit <$> parseInt

symbolRef :: Parser Char String
symbolRef = some $ parseAnyChar $ ['A'..'Z'] ++ ['a'..'z'] ++ "+-*/%$#?=<>"

symbol :: Parser Char SExpr
symbol = Symbol <$> symbolRef

list :: Parser Char SExpr
list = List <$> parseList parseSExpr

string :: Parser Char SExpr
string = StringLit <$> parseString

parseSExpr :: Parser Char SExpr
parseSExpr = useless >> integerLit <|> symbol <|> list <|> string

useless :: Parser Char [String]
useless = many ((parseAnyChar " \n\t" >> return "") <|> comment <|> lineComment)

lineComment :: Parser Char String
lineComment = parseChar ';' >> many (parseAnyButChar '\n') >> return ""

comment :: Parser Char String
comment = commentStart >> many (commentContent) >> commentEnd >> return ""

commentStart :: Parser Char Char
commentStart = parseChar '#' >> parseChar '|'

commentContent :: Parser Char String
commentContent = comment <|> nonComment
  where nonComment = parseNot commentEnd >> parseAny >> return ""

commentEnd :: Parser Char Char
commentEnd = parseChar '|' >> parseChar '#'
