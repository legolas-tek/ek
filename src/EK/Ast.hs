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
  , Prec
  , patternToName
  , defaultPrec
  , precedence
  ) where

import Data.List (intercalate)
import Data.String (IsString(..))
import Data.Maybe (fromMaybe)

data Symbol
  = Symbol String
  | Placeholder
  deriving (Eq)

data FunctionName = FunctionName [Symbol] Prec
  deriving (Eq)

data Expr
  = IntegerLit Integer
  | StringLit String
  | Call FunctionName [Expr]
  | Lambda String Expr
  | StructLit String [Expr]
  deriving (Eq)

data CallItem
  = ExprCall Expr
  | PlaceholderCall
  deriving (Eq)

data Stmt expr
  = AtomDef String
  | TypeDef String Type
  | StructDef String [StructElem]
  | FuncDef FuncPattern expr
  | ExternDef FuncPattern
  deriving (Eq)

data StructElem = StructElem String Type
  deriving (Eq)

data Type
  = TypeName String
  | IntRange (Maybe Integer) (Maybe Integer)
  | UnionType Type Type
  | FunctionType Type Type
  deriving (Eq)

type Prec = Int

data FuncPattern = FuncPattern
  { funcPatternItems :: [FuncPatternItem]
  , funcPatternType :: Maybe Type
  , funcPatternPrec :: Maybe Prec
  } deriving (Eq)

data FuncPatternItem
  = ArgPattern Bool String (Maybe Type)
  | SymbolPattern String
  | PlaceholderPattern
  deriving (Eq)

patternToName :: FuncPattern -> FunctionName
patternToName (FuncPattern items _ prec) = FunctionName (map patternToName' items) (defaultPrec `fromMaybe` prec)
  where
    patternToName' (ArgPattern {}) = Placeholder
    patternToName' (SymbolPattern s) = Symbol s
    patternToName' PlaceholderPattern = Placeholder

defaultPrec :: Prec
defaultPrec = 9 -- same as haskell

instance IsString FunctionName where
  fromString str = FunctionName (unshow <$> words str) defaultPrec
    where unshow "_" = Placeholder
          unshow s = Symbol s

instance Show FunctionName where
  show (FunctionName symbols prec) = unwords (show <$> symbols)
    ++ (if prec /= defaultPrec then " precedence " ++ show prec else "")

precedence :: FunctionName -> Prec -> FunctionName
precedence (FunctionName symbols _) = FunctionName symbols

instance Show Symbol where
  show (Symbol s) = s
  show Placeholder = "_"

instance Show Expr where
  show (IntegerLit i) = show i
  show (StringLit s) = show s
  show (Call (FunctionName name _) items) = "(" ++ unwords (showCall name items) ++ ")"
  show (Lambda arg expr) = "(\\" ++ arg ++ " = " ++ show expr ++ ")"
  show (StructLit s elems) = s ++ " { " ++ intercalate ", " (show <$> elems) ++ " }"

showCall :: [Symbol] -> [Expr] -> [String]
showCall ((Symbol s):xs) i = s : showCall xs i
showCall (Placeholder:xs) (e:is) = show e : showCall xs is
showCall [] _ = []
showCall _ [] = ["#error#"]

instance Show expr => Show (Stmt expr) where
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
  show (FunctionType a b) = show a ++ " -> " ++ show b

instance Show FuncPattern where
  show (FuncPattern items typ prec) = unwords (show <$> items) ++ showType typ ++ showPrec prec
    where showPrec (Just p) = " precedence " ++ show p
          showPrec Nothing = ""
          showType (Just t) = " : " ++ show t
          showType Nothing = ""

instance Show FuncPatternItem where
  show (ArgPattern lazy s (Just t)) = "(" ++ (if lazy then "lazy " else "") ++ s ++ " : " ++ show t ++ ")"
  show (ArgPattern lazy s Nothing) = "(" ++ (if lazy then "lazy " else "") ++ s ++ ")"
  show (SymbolPattern s) = s
  show PlaceholderPattern = "_"

instance Functor Stmt where
  fmap f (FuncDef pat expr) = FuncDef pat (f expr)
  fmap _ (AtomDef s) = AtomDef s
  fmap _ (TypeDef s t) = TypeDef s t
  fmap _ (StructDef s elems) = StructDef s elems
  fmap _ (ExternDef pat) = ExternDef pat

instance Traversable Stmt where
  traverse f (FuncDef pat expr) = FuncDef pat <$> f expr
  traverse _ (AtomDef s) = pure $ AtomDef s
  traverse _ (TypeDef s t) = pure $ TypeDef s t
  traverse _ (StructDef s elems) = pure $ StructDef s elems
  traverse _ (ExternDef pat) = pure $ ExternDef pat

instance Foldable Stmt where
  foldMap f (FuncDef _ expr) = f expr
  foldMap _ _ = mempty
