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

--- Function definition

funcDef :: Parser Token Stmt
funcDef = parseTokenType FnKw >> undefined

externDef :: Parser Token Stmt
externDef = parseTokenType ExternKw >> parseTokenType FnKw >> ExternDef <$> funcPattern

--- Function pattern

funcPattern :: Parser Token FuncPattern
funcPattern = liftM2 FuncPattern (some funcPatternItem) (optional typed)

funcPatternItem :: Parser Token FuncPatternItem
funcPatternItem = placeholder PlaceholderPattern <|> (SymbolPattern <$> identifier) <|> argumentPatternItem

argumentPatternItem :: Parser Token FuncPatternItem
argumentPatternItem = do
  parseTokenType ParenOpen
  name <- identifier
  t <- optional typed
  parseTokenType ParenClose
  return $ ArgPattern name t

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
intType = single <$> intLiteral
  where single i = IntRange (Just i) (Just i)

intRange :: Parser Token Type
intRange = do
  parseTokenType BracketOpen
  l <- optional intLiteral
  parseTokenType DotDot
  u <- optional intLiteral
  parseTokenType BracketClose
  return $ IntRange l u

--- Token parsers

textIdentifier :: Parser Token String
textIdentifier = lexeme <$> parseTokenType TextIdentifier

intLiteral :: Parser Token Integer
intLiteral = read . lexeme <$> parseTokenType IntLiter

placeholder :: a -> Parser Token a
placeholder a = parseTokenType UnderScore >> return a

operatorIdentifier :: Parser Token String
operatorIdentifier = lexeme <$> parseTokenType OperatorIdentifier

identifier :: Parser Token String
identifier = textIdentifier <|> operatorIdentifier

--- Low level parsers

parseTokenType :: TokenType -> Parser Token Token
parseTokenType expected = parseOneIf predicate
  where predicate (Token { tokenType = tt }) = expected == tt
