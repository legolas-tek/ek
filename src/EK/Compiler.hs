{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Compiler
--}

module EK.Compiler
  ( compileToVM
  , Result
  ) where

import VirtualMachine hiding (Env)
import EK.Ast
import Data.Map (Map, fromList, empty, union, toList)
import Data.List (elemIndex)
import Data.Functor ((<&>))

type Env = [String]
type Result = Map String Insts

showBytecode :: Result -> String
showBytecode result = concatMap showEntry (toList result)
    where showEntry (key, value) = key ++ ": " ++ show value ++ "\n"

compileToVM :: [Stmt Expr] -> Either String Result
compileToVM stmts = compileStmts stmts []

compileStmts :: [Stmt Expr] -> Env -> Either String Result
compileStmts arr env = mapM (compileStmt env) arr <&> foldr union empty

addArgToEnv :: FuncPatternItem -> Env
addArgToEnv (ArgPattern _ name _) = [name]
addArgToEnv (SymbolPattern _) = []
addArgToEnv PlaceholderPattern = []

addArgsToEnv :: FuncPattern -> Env -> Env
addArgsToEnv (FuncPattern items _ _) initEnv = concatMap addArgToEnv items ++ initEnv

compileStmt :: Env -> Stmt Expr -> Either String Result
compileStmt env (FuncDef pattern expr) = do
  exprInsts <- compileExpr expr (addArgsToEnv pattern env)
  return (fromList [(show pattern, exprInsts ++ [Ret])])
compileStmt _ _ = Right empty

compileExpr :: Expr -> Env -> Either String Insts
compileExpr (IntegerLit i) _ = Right [Push (IntegerValue i)]
compileExpr (StringLit s) _ = Right [Push (StringValue s)]
compileExpr (EK.Ast.Call name callItems) env =
  compileCallItems callItems env >>= \insts ->
    return (case elemIndex (show name) env of
      Just i -> LoadArg i : insts
      Nothing -> GetEnv (show name) : insts)

compileCallItems :: [Expr] -> Env -> Either String Insts
compileCallItems items env = concat <$> mapM (`compileCallItem` env) items

compileCallItem :: Expr -> Env -> Either String Insts
compileCallItem expr env = compileExpr expr env <&> (++ [VirtualMachine.Call])
