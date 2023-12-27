{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Ast for ek
--}

module EK.Ast
  ( Expr(..)
  , Stmt(..)
  , Symbol(..)
  , FunctionName(..)
  , CallItem(..)
  , StructElem(..)
  , Type(..)
  , FuncPattern(..)
  , FuncPatternItem(..)
  ) where

import Data.List (intercalate)

data Symbol
  = Symbol String
  | Placeholder

newtype FunctionName = FunctionName [Symbol]

data Expr
  = IntegerLit Integer
  | StringLit String
  | Call FunctionName [CallItem]

data CallItem
  = ExprCall Expr
  | PlaceholderCall

data Stmt
  = AtomDef String
  | TypeDef String Type
  | StructDef String [StructElem]
  | FuncDef FuncPattern Expr
  | ExternDef FuncPattern

data StructElem = StructElem String Type

data Type
  = TypeName String
  | IntRange (Maybe Integer) (Maybe Integer)
  | UnionType Type Type

data FuncPattern = FuncPattern [FuncPatternItem] (Maybe Type)

data FuncPatternItem
  = ArgPattern String (Maybe Type)
  | SymbolPattern String
  | PlaceholderPattern

instance Show FunctionName where
  show (FunctionName symbols) = unwords (show <$> symbols)

instance Show Symbol where
  show (Symbol s) = s
  show Placeholder = "_"

instance Show Expr where
  show (IntegerLit i) = show i
  show (StringLit s) = show s
  show (Call (FunctionName name) items) = unwords $ showCall name items

showCall :: [Symbol] -> [CallItem] -> [String]
showCall ((Symbol s):xs) i = s : showCall xs i
showCall (Placeholder:xs) ((ExprCall e):is) = ("(" ++ show e ++ ")") : showCall xs is
showCall (Placeholder:xs) (PlaceholderCall:is) = "_" : showCall xs is
showCall [] _ = []
showCall _ [] = ["#error#"]

instance Show Stmt where
  show (AtomDef s) = "atom " ++ s
  show (TypeDef s t) = "type " ++ s ++ " = " ++ show t
  show (StructDef s []) = "struct " ++ s ++ " {}"
  show (StructDef s elems) = "struct " ++ s ++ " { " ++ intercalate ", " (show <$> elems) ++ " }"
  show (FuncDef pattern expr) = "fn " ++ show pattern ++ " = " ++ show expr
  show (ExternDef pattern) = "extern fn " ++ show pattern
  
instance Show StructElem where
  show (StructElem s t) = s ++ " : " ++ show t

instance Show Type where
  show (TypeName s) = s
  show (IntRange (Just a) (Just b)) = "[" ++ show a ++ ".." ++ show b ++ "]"
  show (IntRange (Just a) Nothing) = "[" ++ show a ++ "..]"
  show (IntRange Nothing (Just b)) = "[.." ++ show b ++ "]"
  show (IntRange Nothing Nothing) = "[..]"
  show (UnionType a b) = show a ++ " | " ++ show b

instance Show FuncPattern where
  show (FuncPattern items (Just t)) = unwords (show <$> items) ++ " : " ++ show t
  show (FuncPattern items Nothing) = unwords (show <$> items)
  
instance Show FuncPatternItem where
  show (ArgPattern s (Just t)) = "(" ++ s ++ " : " ++ show t ++ ")"
  show (ArgPattern s Nothing) = "(" ++ s ++ ")"
  show (SymbolPattern s) = s
  show PlaceholderPattern = "_"
