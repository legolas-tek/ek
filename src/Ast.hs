{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Ast
--}

module Ast
    ( Ast(..)
    , sexprToAST
    ) where

import qualified SExpr

data Ast = Define String Ast
          | IntegerLit Integer
          | Symbol String
          | Lambda [String] [Ast]
          | Call Ast [Ast]
          deriving (Show)

type SyntaxError = String

sexprToAST :: SExpr.SExpr -> Either SyntaxError Ast
sexprToAST (SExpr.Symbol s) = Right (Ast.Symbol s)
sexprToAST (SExpr.IntegerLit i) = Right (Ast.IntegerLit i)
sexprToAST (SExpr.List [SExpr.Symbol "define", SExpr.Symbol s, v]) =
  sexprToAST v >>= \e -> Right (Ast.Define s e)
sexprToAST (SExpr.List (SExpr.Symbol "lambda":(SExpr.List args):xs)) = do
  argsList <- mapM SExpr.getSymbol args
  body <- mapM sexprToAST xs
  return (Ast.Lambda argsList body)
sexprToAST (SExpr.List (SExpr.Symbol x:xs)) = mapM sexprToAST xs >>=
  \e -> Right (Call (Ast.Symbol x) e)
sexprToAST _ = Left "Invalid syntax"
