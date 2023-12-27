{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- expression parser for ek
-}

module EK.ExprParser
  ( parseExprs
  , TotalStmt
  , PartialStmt
  ) where

import EK.Ast
import Token
import Parser
import EK.TokenParser

type TotalStmt = EK.Ast.Stmt Expr
type PartialStmt = EK.Ast.Stmt [Token]

type FuncItem = FunctionName
type Prec = Int

lowestPrec :: Prec
lowestPrec = 0

parseExprs :: [PartialStmt] -> Either String [TotalStmt]
parseExprs partials = mapM (mapM $ parseExpr partials) partials

parseExpr :: [PartialStmt] -> [Token] -> Either String Expr
parseExpr partials tokens = fst <$> runParser (parsePrec (funcItems partials) lowestPrec <* eof) tokens

funcItems :: [PartialStmt] -> [FuncItem]
funcItems (FuncDef pat _ : xs) = patternToName pat : funcItems xs
funcItems (ExternDef pat : xs) = patternToName pat : funcItems xs
funcItems (_ : xs) = funcItems xs

parsePrec :: [FuncItem] -> Int -> Parser Token Expr
parsePrec funcItems prec = prim funcItems

prim :: [FuncItem] -> Parser Token Expr
prim funcItems = intExpr <|> stringExpr <|> parenExpr funcItems

intExpr :: Parser Token Expr
intExpr = IntegerLit <$> intLiteral

stringExpr :: Parser Token Expr
stringExpr = StringLit <$> stringLiteral

parenExpr :: [FuncItem] -> Parser Token Expr
parenExpr funcItems = parseTokenType ParenOpen *> parsePrec funcItems lowestPrec <* parseTokenType ParenClose
