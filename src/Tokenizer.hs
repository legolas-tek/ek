{-
-- epitech project, 2023
-- glados
-- file description:
-- Tokenizer
-}

module Tokenizer
    (
    ) where

import Token (Token(..), TokenType(..))
import Parser

--tokenizer :: [Char] -> String -> [Token]
--tokenizer content fileName = parse (tokenize content) fileName

--parseCurlyOpen :: Parser Char Token
--parseCurlyOpen = parseChar '{' >> return (Token {lexeme = , tokenType = CurlyOpen})

--token :: Parser Char Token
--token = useless >> parseAtomKw

useless :: Parser Char [String]
useless = many ((parseAnyChar " \n\t" >> return "") <|> comment <|> lineComment)

lineComment :: Parser Char String
lineComment = parseChar '/' >> parseChar '/' >> many (parseAnyButChar '\n') >> return ""

comment :: Parser Char String
comment = commentStart >> many commentContent >> commentEnd >> return ""

commentStart :: Parser Char Char
commentStart = parseChar '/' >> parseChar '*'

commentContent :: Parser Char String
commentContent = comment <|> nonComment
  where nonComment = parseNot commentEnd >> parseAny >> return ""

commentEnd :: Parser Char Char
commentEnd = parseChar '*' >> parseChar '/'
