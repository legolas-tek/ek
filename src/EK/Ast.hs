{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Ast for ek
--}

module EK.Ast
  ( Expr(..)
  , Stmt(..)
  ) where

data Expr
  = IntegerLit Integer
  | StringLit String
  | Symbol String
  | Call Expr [Expr]
  deriving (Show)

data Stmt
  = Define String Expr
  | If Expr Stmt Stmt
  | EKExpr Expr
  deriving (Show)
