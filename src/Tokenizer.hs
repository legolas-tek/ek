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

parseCurlyOpen :: Parser Char (String, TokenType)
parseCurlyOpen = parseChar '{' >> return ("{", CurlyOpen)

parseCurlyClose :: Parser Char (String, TokenType)
parseCurlyClose = parseChar '}' >> return ("}", CurlyClose)

parseComma :: Parser Char (String, TokenType)
parseComma = parseChar ',' >> return (",", Comma)

parseUnderscore :: Parser Char (String, TokenType)
parseUnderscore = parseChar '_' >> return ("_", UnderScore)

parseParenOpen :: Parser Char (String, TokenType)
parseParenOpen = parseChar '(' >> return ("(", ParenOpen)

parseParenClose :: Parser Char (String, TokenType)
parseParenClose = parseChar ')' >> return (")", ParenClose)

parseColon :: Parser Char (String, TokenType)
parseColon = parseChar ':' >> return (":", Colon)

parseColonColon :: Parser Char (String, TokenType)
parseColonColon = parseString "::" >> return ("::", ColonColon)

parseBracketOpen :: Parser Char (String, TokenType)
parseBracketOpen = parseChar '[' >> return ("[", BracketOpen)

parseBracketClose :: Parser Char (String, TokenType)
parseBracketClose = parseChar ']' >> return ("]", BracketClose)

parseIntLiter :: Parser Char (String, TokenType)
parseIntLiter = parseInt >>= \integer -> return (show integer, IntLiter)

parseStringLiter :: Parser Char (String, TokenType)
parseStringLiter = parseStringLit >>= \string -> return (string, StringLiter)

parseTextIdentifer :: Parser Char (String, TokenType)
parseTextIdentifer = many (parseOneIf isLetter) >>= \identifier -> return $ case identifier of
    "atom" -> ("atom", AtomKw)
    "struct" -> ("struct", StructKw)
    "type" -> ("type", TypeKw)
    "fn" -> ("fn", FnKw)
    "extern" -> ("extern", ExternKw)
    _ -> (identifier, TextIdentifier)

parseOperatorId :: Parser Char (String, TokenType)
parseOperatorId = many (parseOneIf (`elem` ".=/-+*!?%<>&|^~")) >>= \identifier -> return $ case identifier of
    "=" -> ("=", Equal)
    "|" -> ("|", Pipe)
    ".." -> ("..", DotDot)
    "->" -> ("->", Arrow)
    _ -> (identifier, OperatorIdentifier)


-- Tokenizer

findTokenType :: Parser Char (String, TokenType)
findTokenType = parseCurlyOpen <|> parseCurlyClose <|> parseComma <|> parseUnderscore <|> parseParenOpen
    <|> parseParenClose <|> parseColonColon <|> parseColon <|> parseBracketOpen
    <|> parseBracketClose <|> parseIntLiter <|> parseStringLiter <|> parseTextIdentifer <|> parseOperatorId

token :: Parser Char Token
token = do
    _ <- useless
    pos <- getPos
    tokenT <- findTokenType
    return $ Token (fst tokenT) pos (snd tokenT)

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
