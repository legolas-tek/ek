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
  = IntLit Integer
  | StringLit String
  | Call String [Expr]
  | ParenExpr Expr
  | Placeholder
  deriving (Show)

data Stmt
  = AtomDef String
  | Define String Expr
  | EKExpr Expr
  | TypeDef String Type
  | StructDef String [StructElem]
  | FuncDef FuncPattern Expr
  | ExternFunc FuncPattern
  deriving (Show)

data StructElem = StructElem String Type
  deriving (Show)

data Type
  = TypeName String
  | IntRange (Maybe Integer) (Maybe Integer)
  | UnionType Type Type
  deriving (Show)

data FuncPattern = FuncPattern [FuncPatternItem] (Maybe Type)
  deriving (Show)

data FuncPatternItem
  = ArgPattern String (Maybe Type)
  | IdentifierPattern String
  | PlaceholderPattern
  deriving (Show)
