{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- ek parser
-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module EK.Parser
    ( parseDocument
    , parseSimpleDocument
    ) where

import EK.Ast
import EK.ExprParser
import Parser
import Token
import Tokenizer
import EK.TokenParser
import Data.Maybe (isJust)
import Control.Monad (liftM2, liftM3)
import Diagnostic

parseDocument :: [Token] -> IO [TotalStmt]
parseDocument tokens = parse >>= getImportedTokens . fst >>= exprParse
    where
        parse = either (fail . show) return $ runParser document tokens
        exprParse = either (fail . show) return . parseExprs

parseSimpleDocument :: [Token] -> Either Diagnostic [TotalStmt]
parseSimpleDocument tokens = runParser document tokens >>= parseExprs . fst

--- Statements

document :: Parser Token [PartialStmt]
document = many stmt <* eof

stmt :: Parser Token PartialStmt
stmt = atomDef <|> typeDef <|> structDef <|> funcDef <|> externDef <|> importDef

atomDef :: Parser Token PartialStmt
atomDef = parseTokenType AtomKw >> AtomDef <$> textIdentifier

typeDef :: Parser Token PartialStmt
typeDef = do
  parseTokenType TypeKw
  name <- textIdentifier
  parseTokenType Equal
  TypeDef name <$> typeId

structDef :: Parser Token PartialStmt
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

importDef :: Parser Token PartialStmt
importDef = parseTokenType ImportKw >> ImportDef <$> textIdentifier

-- structElem separated by commas, with an optional trailing comma
structElems :: Parser Token [StructElem]
structElems = structElems' <|> (pure <$> structElem) <|> return []
  where
    structElems' = liftM2 (:) structElem (parseTokenType Comma >> structElems)

--- Function definition

funcDef :: Parser Token PartialStmt
funcDef = parseTokenType FnKw >> FuncDef <$> funcPattern <*> funcBody

funcBody :: Parser Token [Token]
funcBody = parseTokenType Equal >> some parseNotStmtStart

externDef :: Parser Token PartialStmt
externDef = parseTokenType ExternKw >> parseTokenType FnKw >> ExternDef <$> funcPattern

--- Function pattern

funcPattern :: Parser Token FuncPattern
funcPattern = liftM3 FuncPattern (some funcPatternItem) (optional typed) (optional precedenceClause)

funcPatternItem :: Parser Token FuncPatternItem
funcPatternItem = placeholder PlaceholderPattern <|> (SymbolPattern <$> identifier) <|> argumentPatternItem

argumentPatternItem :: Parser Token FuncPatternItem
argumentPatternItem = do
  parseTokenType ParenOpen
  lazy <- optional (parseTokenType LazyKw)
  name <- identifier
  t <- optional typed
  parseTokenType ParenClose
  return $ ArgPattern (isJust lazy) name t

precedenceClause :: Parser Token Prec
precedenceClause = parseTokenType PrecedenceKw >> fromInteger <$> intLiteral

--- Types

typed :: Parser Token Type
typed = parseTokenType Colon >> typeId

typeId :: Parser Token Type
typeId = do
  t <- typeIdButNotArrow
  functionReturn <- optional (parseTokenType Arrow >> typeId)
  return $ createFunction t functionReturn
    where createFunction t Nothing = t
          createFunction t (Just t') = FunctionType t t'

typeIdButNotArrow :: Parser Token Type
typeIdButNotArrow = do
  t <- primType
  next <- optional (parseTokenType Pipe >> typeIdButNotArrow)
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

-- Import Handling

getImportedTokens :: [PartialStmt] -> IO [PartialStmt]
getImportedTokens = fmap concat . mapM handleImportDef

handleImportDef :: PartialStmt -> IO [PartialStmt]
handleImportDef (ImportDef x) = readFile (x ++ ".ek") >>= either (fail . show) return . parseImportedTokens x >>= getImportedTokens
handleImportDef x = return [x]

parseImportedTokens :: String -> String -> Either Diagnostic [PartialStmt]
parseImportedTokens fileName content = fst <$> (tokenizer fileName content >>= runParser document)
