{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- ek parser
-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module EK.Parser (parseDocument) where

import EK.Ast
import Parser
import Token
import Control.Monad (liftM2)

parseDocument :: Parser Token [Stmt]
parseDocument = many stmt

--- Statements

stmt :: Parser Token Stmt
stmt = atomDef <|> typeDef <|> structDef <|> funcDef <|> externDef

atomDef :: Parser Token Stmt
atomDef = parseTokenType AtomKw >> AtomDef <$> textIdentifier

typeDef :: Parser Token Stmt
typeDef = do
  parseTokenType TypeKw
  name <- textIdentifier
  parseTokenType Equal
  TypeDef name <$> typeId

structDef :: Parser Token Stmt
structDef = do
  parseTokenType StructKw
  name <- textIdentifier
  parseTokenType CurlyOpen
  elems <- structElems
  parseTokenType CurlyClose
  return $ StructDef name elems

structElem :: Parser Token StructElem
structElem = do
  name <- textIdentifier
  StructElem name <$> typed

-- structElem separated by commas, with an optional trailing comma
structElems :: Parser Token [StructElem]
structElems = structElems' <|> (pure <$> structElem) <|> return []
  where
    structElems' = liftM2 (:) structElem (parseTokenType Comma >> structElems)

-- Function definition

funcDef :: Parser Token Stmt
funcDef = parseTokenType FnKw >> undefined

externDef :: Parser Token Stmt
externDef = parseTokenType ExternKw >> parseTokenType FnKw >> undefined

--- Types

typed :: Parser Token Type
typed = parseTokenType Colon >> typeId

typeId :: Parser Token Type
typeId = do
  t <- primType
  next <- optional (parseTokenType Pipe >> typeId)
  return $ combine t next
    where combine t Nothing = t
          combine t (Just t') = UnionType t t'

primType :: Parser Token Type
primType = typeName <|> intType <|> intRange

typeName :: Parser Token Type
typeName = TypeName <$> textIdentifier

intType :: Parser Token Type
intType = single . read . lexeme <$> parseTokenType IntLiter
  where single i = IntRange (Just i) (Just i)

intRange :: Parser Token Type
intRange = do
  parseTokenType BracketOpen
  l <- optional $ read . lexeme <$> parseTokenType IntLiter
  parseTokenType DotDot
  u <- optional $ read . lexeme <$> parseTokenType IntLiter
  parseTokenType BracketClose
  return $ IntRange l u

--- Token parsers

textIdentifier :: Parser Token String
textIdentifier = lexeme <$> parseTokenType TextIdentifier

--- Low level parsers

parseTokenType :: TokenType -> Parser Token Token
parseTokenType expected = parseOneIf predicate
  where predicate (Token { tokenType = tt }) = expected == tt
