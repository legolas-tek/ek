{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- ek parser
-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module EK.TypeParser
    ( typed
    , typeId
    ) where

import Parser
import EK.Ast
import Token
import EK.TokenParser

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
primType = typeName <|> intType <|> intRange <|> parenType

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

parenType :: Parser Token Type
parenType = parseTokenType ParenOpen *> typeId <* parseTokenType ParenClose
