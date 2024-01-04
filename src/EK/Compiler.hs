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
compileFuncDefs (stmt:rest) env =
  case compileFuncDef stmt env of
    Left err -> Left err
    Right insts -> do
      restInsts <- compileFuncDefs rest env
      return (insts ++ restInsts)

addArgToEnv :: Env -> FuncPatternItem -> Env
addArgToEnv env (ArgPattern _ name _) = name : env
addArgToEnv env (SymbolPattern _) = env
addArgToEnv env PlaceholderPattern = env

addArgsToEnv :: FuncPattern -> Env -> Env
addArgsToEnv (FuncPattern items _ _) initEnv = foldl addArgToEnv initEnv items

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
    Nothing -> PopEnv (show name) : callInsts)

compileCallItems :: [CallItem] -> Env -> Either String Insts
compileCallItems items env = concat <$> mapM (`compileCallItem` env) items

compileCallItem :: CallItem -> Env -> Either String Insts
compileCallItem (ExprCall expr) env = compileExpr expr env >>= \insts -> return (insts ++ [VirtualMachine.Call])
compileCallItem PlaceholderCall _ = Right []
