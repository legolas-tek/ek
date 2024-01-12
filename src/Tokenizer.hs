{-
-- epitech project, 2023
-- glados
-- file description:
-- Tokenizer
-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Tokenizer
    (
    tokenizer,
    ) where

import Token
import Parser

import Data.Char (isLetter, isDigit)
import Diagnostic

type TokenizerError = Diagnostic

tokens :: Parser Char [Token]
tokens = many token <* useless <* eof

tokenizer :: String -> String -> Either TokenizerError ([Token], [Diagnostic])
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

parseBackslash :: Parser Char (String, TokenType)
parseBackslash = extractTokenType "\\" Backslash

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

parseFloatLiter :: Parser Char (String, TokenType)
parseFloatLiter = parseFloat >>= \float -> return (show float, FloatLiter)

parseStringLiter :: Parser Char (String, TokenType)
parseStringLiter = parseStringLit >>= \string -> return (string, StringLiter)

parseTextIdentifer :: Parser Char (String, TokenType)
parseTextIdentifer = (:) <$> identifierHead <*> many identifierChar >>= tup
  where tup identifier = return (identifier, identifyKw identifier)
        identifierHead = parseOneIf isLetter
        identifierChar = parseOneIf (\c -> isLetter c || isDigit c || c == '\'')

identifyKw :: String -> TokenType
identifyKw "atom" = AtomKw
identifyKw "struct" = StructKw
identifyKw "type" = TypeKw
identifyKw "fn" = FnKw
identifyKw "extern" = ExternKw
identifyKw "precedence" = PrecedenceKw
identifyKw "import" = ImportKw
identifyKw "lazy" = LazyKw
identifyKw _ = TextIdentifier

parseOperatorId :: Parser Char (String, TokenType)
parseOperatorId = some (parseOneIf (`elem` ".=/-+*!?%<>&|^~$")) >>= tup
  where tup identifier = return (identifier, identifyOp identifier)

identifyOp :: String -> TokenType
identifyOp "=" = Equal
identifyOp "|" = Pipe
identifyOp ".." = DotDot
identifyOp "->" = Arrow
identifyOp _ = OperatorIdentifier

-- Tokenizer

findTokenType :: Parser Char (String, TokenType)
findTokenType = parseCurlyOpen <|> parseCurlyClose <|> parseComma <|> parseBackslash <|> parseUnderscore <|> parseParenOpen
    <|> parseParenClose <|> parseColonColon <|> parseColon <|> parseBracketOpen
    <|> parseBracketClose <|> parseFloatLiter <|> parseIntLiter <|> parseStringLiter <|> parseTextIdentifer <|> parseOperatorId

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

commentStart :: Parser Char String
commentStart = parseString "/*"

commentContent :: Parser Char String
commentContent = comment <|> nonComment
  where nonComment = parseNot commentEnd >> parseAny >> return ""

commentEnd :: Parser Char String
commentEnd = parseString "*/"
