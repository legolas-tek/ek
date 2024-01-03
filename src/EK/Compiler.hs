{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Compiler
--}

module EK.Compiler
  ( compileToVM
  ) where

import VirtualMachine
import EK.Ast

import Data.Map (empty)

compileToVM :: [Stmt Expr] -> Either String Insts
compileToVM stmts = compileFuncDefs stmts empty

compileFuncDefs :: [Stmt Expr] -> Env -> Either String Insts
compileFuncDefs [] _ = Right []
compileFuncDefs (stmt:rest) env =
  case compileFuncDef stmt env of
    Left err -> Left err
    Right insts -> do
      restInsts <- compileFuncDefs rest env
      return (insts ++ restInsts)

compileFuncDef :: Stmt Expr -> Env -> Either String Insts
compileFuncDef (FuncDef pattern expr) env = do
  exprInsts <- compileExpr expr env
  let patternInsts = compilePattern pattern env
  return (patternInsts ++ exprInsts ++ [Ret])
compileFuncDef _ _ = Right []

compilePattern :: FuncPattern -> Env -> Insts
compilePattern (FuncPattern items _ _) _ =
  concatMap compilePatternItem items
  where
    compilePatternItem (ArgPattern _ _ _) = []
    compilePatternItem (SymbolPattern _) = []
    compilePatternItem PlaceholderPattern = []

compileExpr :: Expr -> Env -> Either String Insts
compileExpr (IntegerLit i) _ = Right [Push (IntegerValue i)]
compileExpr (StringLit s) _ = Right [Push (StringValue s)]
compileExpr (EK.Ast.Call name callItems) env = do
  callInsts <- compileCallItems callItems env
  return (callInsts ++ [PopEnv (show name), VirtualMachine.Call])

compileCallItems :: [CallItem] -> Env -> Either String Insts
compileCallItems items env = concat <$> mapM (`compileCallItem` env) items

compileCallItem :: CallItem -> Env -> Either String Insts
compileCallItem (ExprCall expr) env = compileExpr expr env
compileCallItem PlaceholderCall _ = Right []
