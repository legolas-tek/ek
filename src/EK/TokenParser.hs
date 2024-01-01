{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- expression parser for ek
-}

module EK.TokenParser
  ( textIdentifier
  , intLiteral
  , stringLiteral
  , placeholder
  , operatorIdentifier
  , identifier
  , parseTokenType
  , parseNotStmtStart
  , eof
  , identifierExact
  ) where

import Token
import Parser

textIdentifier :: Parser Token String
textIdentifier = lexeme <$> parseTokenType TextIdentifier

intLiteral :: Parser Token Integer
intLiteral = read . lexeme <$> parseTokenType IntLiter

stringLiteral :: Parser Token String
stringLiteral = lexeme <$> parseTokenType StringLiter

placeholder :: a -> Parser Token a
placeholder a = parseTokenType UnderScore >> return a

operatorIdentifier :: Parser Token String
operatorIdentifier = lexeme <$> parseTokenType OperatorIdentifier

identifier :: Parser Token String
identifier = textIdentifier <|> operatorIdentifier

identifierExact :: String -> Parser Token String
identifierExact s = identifier >>= \t -> if t == s then return s else fail $ "Expected " ++ s ++ " but found " ++ t

--- Low level parsers

parseTokenType :: TokenType -> Parser Token Token
parseTokenType expected = parseOneIf predicate
  where predicate (Token { tokenType = tt }) = expected == tt

parseNotStmtStart :: Parser Token Token
parseNotStmtStart = parseOneIf predicate
  where predicate (Token { tokenType = tt }) = tt /= AtomKw && tt /= TypeKw && tt /= StructKw && tt /= FnKw && tt /= ExternKw

eof :: Parser Token ()
eof = const "Unexpected trailing token" `mapError` parseNot (parseOneIf $ const True)
