{-
-- epitech project, 2023
-- glados
-- file description:
-- Tokenizer
-}

module Tokenizer
    (
    ) where

import Token
import Parser

import Data.Char (isLetter)

-- Identify the token type

parseCurlyOpen :: Parser Char TokenType
parseCurlyOpen = parseChar '{' >> return CurlyOpen

parseCurlyClose :: Parser Char TokenType
parseCurlyClose = parseChar '}' >> return CurlyClose

parseComma :: Parser Char TokenType
parseComma = parseChar ',' >> return Comma

parseUnderscore :: Parser Char TokenType
parseUnderscore = parseChar '_' >> return UnderScore

parseParenOpen :: Parser Char TokenType
parseParenOpen = parseChar '(' >> return ParenOpen

parseParenClose :: Parser Char TokenType
parseParenClose = parseChar ')' >> return ParenClose

parseColon :: Parser Char TokenType
parseColon = parseChar ':' >> return Colon

parseColonColon :: Parser Char TokenType
parseColonColon = parseString "::" >> return ColonColon

parseBracketOpen :: Parser Char TokenType
parseBracketOpen = parseChar '[' >> return BracketOpen

parseBracketClose :: Parser Char TokenType
parseBracketClose = parseChar ']' >> return BracketClose

parseIntLiter :: Parser Char TokenType
parseIntLiter = parseInt >> return IntLiter

parseStringLiter :: Parser Char TokenType
parseStringLiter = parseStringLit >> return StringLiter

parseTextIdentifer :: Parser Char TokenType
parseTextIdentifer = many (parseOneIf isLetter) >>= \identifier -> return $ case identifier of
    "atom" -> AtomKw
    "struct" -> StructKw
    "type" -> TypeKw
    "fn" -> FnKw
    "extern" -> ExternKw
    _ -> TextIdentifier

parseOperatorId :: Parser Char TokenType
parseOperatorId = many (parseOneIf (`elem` ".=/-+*!?%<>&|^~")) >>= \identifier -> return $ case identifier of
    "=" -> Equal
    "|" -> Pipe
    ".." -> DotDot
    "->" -> Arrow
    _ -> OperatorIdentifier


-- Tokenizer

tokenType :: Parser Char TokenType
tokenType = parseCurlyOpen <|> parseCurlyClose <|> parseComma <|> parseUnderscore <|> parseParenOpen
    <|> parseParenClose <|> parseColonColon <|> parseColon <|> parseBracketOpen
    <|> parseBracketClose <|> parseIntLiter <|> parseStringLiter <|> parseTextIdentifer <|> parseOperatorId

-- Handling of useless characters, comments and line comments

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

