{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Compiler
--}

module EK.Compiler
  ( compileToVM
  ) where

import VirtualMachine hiding (Env)
import EK.Ast
import Data.List (elemIndex)

type Env = [String]

compileToVM :: [Stmt Expr] -> Either String Insts
compileToVM stmts = compileFuncDefs stmts []

compileFuncDefs :: [Stmt Expr] -> Env -> Either String Insts
compileFuncDefs [] _ = Right []
compileFuncDefs (stmt:rest) env = do
  insts <- compileFuncDef stmt env
  restInsts <- compileFuncDefs rest env
  return (insts ++ restInsts)

addArgToEnv :: FuncPatternItem -> Env
addArgToEnv (ArgPattern _ name _) = [name]
addArgToEnv (SymbolPattern _) = []
addArgToEnv PlaceholderPattern = []

addArgsToEnv :: FuncPattern -> Env -> Env
addArgsToEnv (FuncPattern items _ _) initEnv = concatMap addArgToEnv items ++ initEnv

compileFuncDef :: Stmt Expr -> Env -> Either String Insts
compileFuncDef (FuncDef pattern expr) env = do
  exprInsts <- compileExpr expr (addArgsToEnv pattern env)
  return (exprInsts ++ [Ret])
compileFuncDef _ _ = Right []

compileExpr :: Expr -> Env -> Either String Insts
compileExpr (IntegerLit i) _ = Right [Push (IntegerValue i)]
compileExpr (StringLit s) _ = Right [Push (StringValue s)]
compileExpr (EK.Ast.Call name callItems) env = do
  callInsts <- compileCallItems callItems env
  return (case elemIndex (show name) env of
    Just i -> LoadArg i : callInsts
    Nothing -> GetEnv (show name) : callInsts)

compileCallItems :: [CallItem] -> Env -> Either String Insts
compileCallItems items env = concat <$> mapM (`compileCallItem` env) items

compileCallItem :: CallItem -> Env -> Either String Insts
compileCallItem (ExprCall expr) env = compileExpr expr env >>= \insts -> return (insts ++ [VirtualMachine.Call])
compileCallItem PlaceholderCall _ = Right []
