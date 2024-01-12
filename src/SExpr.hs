{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- SExpr
--}

module SExpr
  ( SExpr(..)
  , getSymbol
  , printTree
  ) where

data SExpr = IntegerLit Integer
           | Symbol String
           | List [SExpr]
           | StringLit String
           deriving (Eq, Show)

type SexprError = String

getSymbol :: SExpr -> Either SexprError String
getSymbol (Symbol s) = Right s
getSymbol _ = Left "Not a symbol"

printTree :: SExpr -> Either SexprError String
printTree (Symbol s) = Right ("a Symbol " ++ s)
printTree (IntegerLit i) = Right ("a Number " ++ show i)
printTree (StringLit str) = Right ("a String " ++ show str)
printTree(List l) = mapM printTree l >>=
  \e -> Right ("a List with " ++ unwords e)
