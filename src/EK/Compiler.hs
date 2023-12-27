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

import Data.Map (Map, lookup, empty)

compileToVM :: [Stmt] -> [Expr] -> Either String Insts
compileToVM stmts exprs = do
  stmtInsts <- compileStmts stmts empty
  exprInsts <- compileExprs exprs empty
  return $ stmtInsts ++ exprInsts

compileExprs :: [Expr] -> Env -> Either String Insts
compileExprs [] env = Right []
compileExprs (expr:exprs) env =
  case compileExpr expr env of
    Left err -> Left err
    Right insts -> do
      restInsts <- compileExprs exprs env
      return (insts ++ restInsts)

compileExpr :: Expr -> Env -> Either String Insts
compileExpr (IntegerLit i) _ = Right [Push (IntegerValue i)]
compileExpr (StringLit s) _ = Right [Push (StringValue s)]
compileExpr (EK.Ast.Call name callItems) env = Right []

compileStmts :: [Stmt] -> Env -> Either String Insts
compileStmts [] env = Right []
compileStmts (stmt:rest) env =
  case compileStmt stmt env of
    Left err -> Left err
    Right insts -> do
      restInsts <- compileStmts rest env
      return (insts ++ restInsts)

compileStmt :: Stmt -> Env -> Either String Insts
compileStmt (AtomDef _) env = Right []
compileStmt (TypeDef _ _) env = Right []
compileStmt (StructDef _ _) env = Right []
compileStmt (FuncDef pattern expr) env = Right []
compileStmt (ExternDef pattern) env = Right []
