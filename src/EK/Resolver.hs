{--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Type Resolver
--}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module EK.Resolver
  ( resolveTypes
  ) where

import EK.Ast hiding (Type)
import Diagnostic
import Data.Map (Map)
import qualified Data.Map as Map
import EK.Types
import qualified EK.Ast as Ast
import Data.Maybe (fromMaybe)
import Control.Monad.State
import SourcePos

resolveTypes :: [TotalStmt] -> ([TypedStmt], [Diagnostic])
resolveTypes stmts = replaceTypes stmts $ resolveIncrementally stmts $ collectTypes stmts

resolveIncrementally :: [TotalStmt] -> Map String Type -> Map String Type
resolveIncrementally stmt types = if types == types' then types else resolveIncrementally stmt types'
  where types' = foldl resolveType types stmt

collectTypes :: [TotalStmt] -> Map String Type
collectTypes = (defaultTypes <>) . foldMap collectType

defaultTypes :: Map String Type
defaultTypes = Map.fromList
  [ ("any", AnyTy)
  , ("never", mempty)
  , ("string", structTy "string" [])
  , ("float", structTy "float" [])
  ]

collectType :: TotalStmt -> Map String Type
collectType (AtomDef name) = Map.fromList [(name, atomTy name)]
collectType (TypeDef name _) = Map.fromList [(name, UnresolvedTy)]
collectType (StructDef name _) = Map.fromList [(name, structTy name [])]
collectType _ = Map.empty

resolveType :: Map String Type -> TotalStmt -> Map String Type
resolveType types (TypeDef name ty) = Map.insert name (resolveASTType types ty) types
resolveType types _ = types

resolveASTType :: Map String Type -> Ast.Type -> Type
resolveASTType types (Ast.UnionType a b) = resolveASTType types a <> resolveASTType types b
resolveASTType types (Ast.TypeName name) = fromMaybe UnresolvedTy (Map.lookup name types)
resolveASTType types (Ast.FunctionType arg ret) = functionTy (resolveASTType types arg) (resolveASTType types ret)
resolveASTType _ (Ast.IntRange (Just a) (Just b)) = intRangeTy a b
resolveASTType _ (Ast.IntRange (Just a) Nothing) = intRangeFromTy a
resolveASTType _ (Ast.IntRange Nothing (Just b)) = intRangeUpToTy b
resolveASTType _ (Ast.IntRange Nothing Nothing) = intRangeInfTy

replaceTypes :: [TotalStmt] -> Map String Type -> ([TypedStmt], [Diagnostic])
replaceTypes stmts types = runState (mapM (replaceStmt types) stmts) []

replaceStmt :: Map String Type -> TotalStmt -> State [Diagnostic] TypedStmt
replaceStmt _ (AtomDef name) = return $ AtomDef name
replaceStmt types (TypeDef name ty) = TypeDef name <$> replaceType types ty
replaceStmt types (StructDef name fields) = StructDef name <$> mapM replaceField fields
  where replaceField (StructElem fname ty) = StructElem fname <$> replaceType types ty
replaceStmt types (FuncDef pat body) = FuncDef <$> replacePat types pat <*> replaceExpr types body
replaceStmt _ (ImportDef name) = return $ ImportDef name
replaceStmt types (ExternDef pat) = ExternDef <$> replacePat types pat

replacePat :: Map String Type -> FuncPattern' Ast.Type -> State [Diagnostic] (FuncPattern' Type)
replacePat types (FuncPattern items ret prec) = FuncPattern <$> mapM replaceItem items <*> traverse (replaceType types) ret <*> pure prec
  where replaceItem (ArgPattern lazy name ty) = ArgPattern lazy name <$> traverse (replaceType types) ty
        replaceItem PlaceholderPattern = return PlaceholderPattern
        replaceItem (SymbolPattern name) = return $ SymbolPattern name

replaceType :: Map String Type -> Ast.Type -> State [Diagnostic] Type
replaceType types t = do
  let t' = resolveASTType types t
  when (t' == UnresolvedTy) $ diag $ Diagnostic Error ("Unresolved type " ++ show t) (SourcePos "" 1 1)
  return t'

replaceExpr :: Map String Type -> Expr' Ast.Type -> State [Diagnostic] (Expr' Type)
replaceExpr _ (IntegerLit i) = return $ IntegerLit i
replaceExpr _ (StringLit s) = return $ StringLit s
replaceExpr _ (FloatLit f) = return $ FloatLit f
replaceExpr types (Call f args) = Call f <$> mapM (replaceExpr types) args
replaceExpr types (Lambda name body) = Lambda name <$> replaceExpr types body
replaceExpr types (StructLit ty fields) = StructLit <$> replaceType types ty <*> mapM (replaceExpr types) fields
replaceExpr types (TypeCheck expr ty) = TypeCheck <$> replaceExpr types expr <*> replaceType types ty

diag :: Diagnostic -> State [Diagnostic] ()
diag d = modify (d:)
