{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Ast for ek
--}

module EK.Ast
  ( Expr
  , Expr'(..)
  , Stmt(..)
  , Symbol(..)
  , FunctionName(..)
  , CallItem(..)
  , StructElem
  , StructElem'(..)
  , Type(..)
  , FuncPattern
  , FuncPattern'(..)
  , FuncPatternItem
  , FuncPatternItem'(..)
  , Prec
  , PartialStmt
  , TotalStmt
  , TypedStmt
  , patternToName
  , patternLazinesses
  , defaultPrec
  , precedence
  ) where

import Data.List (intercalate)
import Data.String (IsString(..))
import Data.Maybe (fromMaybe)
import Token
import qualified EK.Types

data Symbol
  = Symbol String
  | Placeholder
  deriving (Eq)

data FunctionName = FunctionName [Symbol] Prec
  deriving (Eq)

data Expr' typeval
  = IntegerLit Integer
  | StringLit String
  | Call FunctionName [Expr' typeval]
  | Lambda String (Expr' typeval)
  | StructLit typeval [Expr' typeval]
  deriving (Eq)

type Expr = Expr' Type

data CallItem
  = ExprCall Expr
  | PlaceholderCall
  deriving (Eq)

data Stmt expr typeval
  = AtomDef String
  | TypeDef String typeval
  | ImportDef String
  | StructDef String [StructElem' typeval]
  | FuncDef (FuncPattern' typeval) expr
  | ExternDef (FuncPattern' typeval)
  deriving (Eq)


data StructElem' typeval = StructElem String typeval
  deriving (Eq)
type StructElem = StructElem' Type

data Type
  = TypeName String
  | IntRange (Maybe Integer) (Maybe Integer)
  | UnionType Type Type
  | FunctionType Type Type
  deriving (Eq)

type PartialStmt = Stmt [Token] Type
type TotalStmt = Stmt Expr Type
type TypedStmt = Stmt (Expr' EK.Types.Type) EK.Types.Type

type Prec = Int

data FuncPattern' typeval = FuncPattern
  { funcPatternItems :: [FuncPatternItem' typeval]
  , funcPatternType :: Maybe typeval
  , funcPatternPrec :: Maybe Prec
  } deriving (Eq)

data FuncPatternItem' typeval
  = ArgPattern Bool String (Maybe typeval)
  | SymbolPattern String
  | PlaceholderPattern
  deriving (Eq)

type FuncPattern = FuncPattern' Type
type FuncPatternItem = FuncPatternItem' Type

patternToName :: FuncPattern' typeval -> FunctionName
patternToName (FuncPattern items _ prec) = FunctionName (map patternToName' items) (defaultPrec `fromMaybe` prec)
  where
    patternToName' (ArgPattern {}) = Placeholder
    patternToName' (SymbolPattern s) = Symbol s
    patternToName' PlaceholderPattern = Placeholder

patternLazinesses :: FuncPattern' a-> [Bool]
patternLazinesses = concatMap patternLazyness . funcPatternItems
  where patternLazyness (ArgPattern l _ _) = [l]
        patternLazyness (SymbolPattern _) = []
        patternLazyness PlaceholderPattern = [False]

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

instance Show typeval => Show (Expr' typeval) where
  show (IntegerLit i) = show i
  show (StringLit s) = show s
  show (Call (FunctionName name _) items) = "(" ++ unwords (showCall name items) ++ ")"
  show (Lambda arg expr) = "(\\" ++ arg ++ " = " ++ show expr ++ ")"
  show (StructLit s elems) = show s ++ " { " ++ intercalate ", " (show <$> elems) ++ " }"

showCall :: Show typeval => [Symbol] -> [Expr' typeval] -> [String]
showCall ((Symbol s):xs) i = s : showCall xs i
showCall (Placeholder:xs) (e:is) = show e : showCall xs is
showCall [] _ = []
showCall _ [] = ["#error#"]

instance (Show expr, Show typeval) => Show (Stmt expr typeval) where
  show (AtomDef s) = "atom " ++ s
  show (TypeDef s t) = "type " ++ s ++ " = " ++ show t
  show (StructDef s []) = "struct " ++ s ++ " {}"
  show (StructDef s elems) = "struct " ++ s ++ " { " ++ intercalate ", " (show <$> elems) ++ " }"
  show (FuncDef pattern expr) = "fn " ++ show pattern ++ " = " ++ show expr
  show (ExternDef pattern) = "extern fn " ++ show pattern
  show (ImportDef s) = "import " ++ s

instance Show ty => Show (StructElem' ty) where
  show (StructElem s t) = s ++ " : " ++ show t

instance Show Type where
  show (TypeName s) = s
  show (IntRange (Just a) (Just b)) = "[" ++ show a ++ ".." ++ show b ++ "]"
  show (IntRange (Just a) Nothing) = "[" ++ show a ++ "..]"
  show (IntRange Nothing (Just b)) = "[.." ++ show b ++ "]"
  show (IntRange Nothing Nothing) = "[..]"
  show (UnionType a b) = "(" ++ show a ++ " | " ++ show b ++ ")"
  show (FunctionType a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

instance Show typeval => Show (FuncPattern' typeval) where
  show (FuncPattern items typ prec) = unwords (show <$> items) ++ showType typ ++ showPrec prec
    where showPrec (Just p) = " precedence " ++ show p
          showPrec Nothing = ""
          showType (Just t) = " : " ++ show t
          showType Nothing = ""

instance Show typeval => Show (FuncPatternItem' typeval) where
  show (ArgPattern lazy s (Just t)) = "(" ++ (if lazy then "lazy " else "") ++ s ++ " : " ++ show t ++ ")"
  show (ArgPattern lazy s Nothing) = "(" ++ (if lazy then "lazy " else "") ++ s ++ ")"
  show (SymbolPattern s) = s
  show PlaceholderPattern = "_"

