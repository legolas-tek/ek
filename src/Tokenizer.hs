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

-- Identify the token type

parseAtomKw :: Parser Char TokenType
parseAtomKw = parseString "atom" >> return AtomKw

parseStructKw :: Parser Char TokenType
parseStructKw = parseString "struct" >> return StructKw

parseTypeKw :: Parser Char TokenType
parseTypeKw = parseString "type" >> return TypeKw

parseFnKw :: Parser Char TokenType
parseFnKw = parseString "fn" >> return FnKw

parseExternKw :: Parser Char TokenType
parseExternKw = parseString "extern" >> return ExternKw

parseEq :: Parser Char TokenType
parseEq = parseChar '=' >> return Equal

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

parsePipe :: Parser Char TokenType
parsePipe = parseChar '|' >> return Pipe

parseBracketOpen :: Parser Char TokenType
parseBracketOpen = parseChar '[' >> return BracketOpen

parseBracketClose :: Parser Char TokenType
parseBracketClose = parseChar ']' >> return BracketClose

parseDotDot :: Parser Char TokenType
parseDotDot = parseString ".." >> return DotDot

parseIntLiter :: Parser Char TokenType
parseIntLiter = parseInt >> return IntLiter

parseStringLiter :: Parser Char TokenType
parseStringLiter = parseStringLit >> return StringLiter

parseTextIdentifer :: Parser Char TokenType
parseTextIdentifer = parseStringLit >> return TextIdentifier

parseOperatorId :: Parser Char TokenType
parseOperatorId = parseOneIf (`elem` "/-+*!?%<>&|^~") >> return OperatorIdentifier


-- Tokenizer

tokenType :: Parser Char TokenType
tokenType = parseAtomKw <|> parseStructKw <|> parseTypeKw <|> parseFnKw <|> parseExternKw <|> parseEq <|> parseCurlyOpen <|> parseCurlyClose
    <|> parseComma <|> parseUnderscore <|> parseParenOpen <|> parseParenClose <|> parseColon <|> parseColonColon <|> parsePipe <|> parseBracketOpen
    <|> parseBracketClose <|> parseDotDot <|> parseIntLiter <|> parseStringLiter <|> parseTextIdentifer <|> parseOperatorId

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

