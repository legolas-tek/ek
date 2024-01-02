{-
-- epitech project, 2023
-- glados
-- file description:
-- Tokenizer
-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Tokenizer
    (
    tokenizer
    ) where

import Token
import Parser

import Data.Char (isLetter)

type TokenizerError = String

tokens :: Parser Char [Token]
tokens = many token <* useless <* eof

tokenizer :: String -> String -> Either TokenizerError [Token]
tokenizer = runParserOnFile tokens

-- Identify the token type

extractTokenType :: String -> TokenType -> Parser Char (String, TokenType)
extractTokenType val tokenTypeVal = parseString val >> return (val, tokenTypeVal)

parseCurlyOpen :: Parser Char (String, TokenType)
parseCurlyOpen = extractTokenType "{" CurlyOpen

parseCurlyClose :: Parser Char (String, TokenType)
parseCurlyClose = extractTokenType "}" CurlyClose

parseComma :: Parser Char (String, TokenType)
parseComma = extractTokenType "," Comma

parseUnderscore :: Parser Char (String, TokenType)
parseUnderscore = extractTokenType "_" UnderScore

parseParenOpen :: Parser Char (String, TokenType)
parseParenOpen = extractTokenType "(" ParenOpen

parseParenClose :: Parser Char (String, TokenType)
parseParenClose = extractTokenType ")" ParenClose

parseColon :: Parser Char (String, TokenType)
parseColon = extractTokenType ":" Colon

parseColonColon :: Parser Char (String, TokenType)
parseColonColon = extractTokenType "::" ColonColon

parseBracketOpen :: Parser Char (String, TokenType)
parseBracketOpen = extractTokenType "[" BracketOpen

parseBracketClose :: Parser Char (String, TokenType)
parseBracketClose = extractTokenType "]" BracketClose

parseIntLiter :: Parser Char (String, TokenType)
parseIntLiter = parseInt >>= \integer -> return (show integer, IntLiter)

parseStringLiter :: Parser Char (String, TokenType)
parseStringLiter = parseStringLit >>= \string -> return (string, StringLiter)

parseTextIdentifer :: Parser Char (String, TokenType)
parseTextIdentifer = many (parseOneIf isLetter) >>= identify
  where identify "atom" = return ("atom", AtomKw)
        identify "struct" = return ("struct", StructKw)
        identify "type" = return ("type", TypeKw)
        identify "fn" = return ("fn", FnKw)
        identify "extern" = return ("extern", ExternKw)
        identify identifier = return (identifier, TextIdentifier)

parseOperatorId :: Parser Char (String, TokenType)
parseOperatorId = many (parseOneIf (`elem` ".=/-+*!?%<>&|^~")) >>= identify
  where identify "=" = return ("=", Equal)
        identify "|" = return ("|", Pipe)
        identify ".." = return ("..", DotDot)
        identify "->" = return ("->", Arrow)
        identify operator = return (operator, OperatorIdentifier)


-- Tokenizer

findTokenType :: Parser Char (String, TokenType)
findTokenType = parseCurlyOpen <|> parseCurlyClose <|> parseComma <|> parseUnderscore <|> parseParenOpen
    <|> parseParenClose <|> parseColonColon <|> parseColon <|> parseBracketOpen
    <|> parseBracketClose <|> parseIntLiter <|> parseStringLiter <|> parseTextIdentifer <|> parseOperatorId

token :: Parser Char Token
token = do
    useless
    pos <- getPos
    (lexemeValue, tokenTypeValue) <- findTokenType
    return $ Token lexemeValue pos tokenTypeValue

-- Handling of useless characters, comments and line comments

useless :: Parser Char [String]
useless = many ((parseAnyChar " \n\t" >> return "") <|> comment <|> lineComment)

lineComment :: Parser Char String
lineComment = parseString "//" >> many (parseAnyButChar '\n') >> return ""

comment :: Parser Char String
comment = commentStart >> many commentContent >> commentEnd >> return ""

commentStart :: Parser Char Char
commentStart = parseChar '/' >> parseChar '*'

commentContent :: Parser Char String
commentContent = comment <|> nonComment
  where nonComment = parseNot commentEnd >> parseAny >> return ""

commentEnd :: Parser Char Char
commentEnd = parseChar '*' >> parseChar '/'
