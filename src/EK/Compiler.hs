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

compileToVM :: [Stmt] -> Either String Insts
compileToVM stmts = compileFuncDefs stmts empty

compileFuncDefs :: [Stmt] -> Env -> Either String Insts
compileFuncDefs [] _ = Right []
compileFuncDefs (stmt:rest) env =
  case compileFuncDef stmt env of
    Left err -> Left err
    Right insts -> do
      restInsts <- compileFuncDefs rest env
      return (insts ++ restInsts)

compileFuncDef :: Stmt -> Env -> Either String Insts
compileFuncDef (FuncDef pattern expr) env = do
  exprInsts <- compileExpr expr env
  let patternInsts = compilePattern pattern env
  return (patternInsts ++ exprInsts ++ [Ret])
compileFuncDef _ _ = Right []

compilePattern :: FuncPattern -> Env -> Insts
compilePattern (FuncPattern items _) _ =
  concatMap compilePatternItem items
  where
    compilePatternItem (ArgPattern s _) = [PushEnv s]
    compilePatternItem (SymbolPattern s) = [PushEnv s]
    compilePatternItem PlaceholderPattern = []

compileExpr :: Expr -> Env -> Either String Insts
compileExpr (IntegerLit i) _ = Right [Push (IntegerValue i)]
compileExpr (StringLit s) _ = Right [Push (StringValue s)]
compileExpr (EK.Ast.Call _ _) _ = Right []
