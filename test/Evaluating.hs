{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

module Evaluating (tests) where

import Test.HUnit

import Ast
import Evaluation

eval :: Ast -> Either EvalError RuntimeValue
eval = (snd <$>) . evalAst defaultEnv

tests :: Test
tests = test
  [ "bools" ~: do
      eval (Symbol "#t") @?= Right (BooleanValue True)
      eval (Symbol "#f") @?= Right (BooleanValue False)
  , "undefinedSymbol" ~: do
      eval (Symbol "ThisDoesntExistByDefault") @?= Left "Symbol ThisDoesntExistByDefault not found"
  , "definedSymbol" ~: do
      (snd <$> evalBody defaultEnv [(Define "myVar" $ IntegerLit 42), (Symbol "myVar")]) @?= Right (IntegerValue 42)
  , "math2" ~: do
      eval (Call (Symbol "*") [IntegerLit 6, IntegerLit 7]) @?= Right (IntegerValue 42)
      eval (Call (Symbol "+") [IntegerLit 9, IntegerLit 3]) @?= Right (IntegerValue 12)
      eval (Call (Symbol "-") [IntegerLit 6, IntegerLit 7]) @?= Right (IntegerValue (-1))
      eval (Call (Symbol "/") [IntegerLit 42, IntegerLit 7]) @?= Right (IntegerValue 6)
      eval (Call (Symbol "/") [IntegerLit 42, IntegerLit 0]) @?= Left "Division by zero"
      eval (Call (Symbol "/") [IntegerLit 42, IntegerLit 3, IntegerLit 3]) @?= Left "List contains more than two elements"
  , "minmax" ~: do
      eval (Call (Symbol "min") [IntegerLit 6, IntegerLit 7]) @?= Right (IntegerValue 6)
      eval (Call (Symbol "max") [IntegerLit 9, IntegerLit 3]) @?= Right (IntegerValue 9)
  , "mathMultiple" ~: do
      eval (Call (Symbol "*") [IntegerLit 6, IntegerLit 7, IntegerLit 2]) @?= Right (IntegerValue 84)
      eval (Call (Symbol "+") [IntegerLit 9, IntegerLit 3, IntegerLit (-2)]) @?= Right (IntegerValue 10)
      eval (Call (Symbol "-") [IntegerLit 20, IntegerLit 7, IntegerLit 0, IntegerLit 4]) @?= Right (IntegerValue 9)
  , "IfEval" ~: do
      eval (If (Symbol "#t") (IntegerLit 0) (IntegerLit 1)) @?= Right (IntegerValue 0)
      eval (If (Symbol "#f") (IntegerLit 0) (IntegerLit 1)) @?= Right (IntegerValue 1)
  , "bool" ~: do
      eval (Call (Symbol "and") [Symbol "#t", Symbol "#t"]) @?= Right (BooleanValue True)
      eval (Call (Symbol "and") [Symbol "#t", Symbol "#t", Symbol "#t", Symbol "#t", Symbol "#f"]) @?= Right (BooleanValue False)
      eval (Call (Symbol "or") [Symbol "#t", Symbol "#t"]) @?= Right (BooleanValue True)
      eval (Call (Symbol "or") [Symbol "#t", Symbol "#f"]) @?= Right (BooleanValue True)
      eval (Call (Symbol "or") [Symbol "#f", Symbol "#f"]) @?= Right (BooleanValue False)
      eval (Call (Symbol "not") [Symbol "#f"]) @?= Right (BooleanValue True)
      eval (Call (Symbol "not") [Symbol "#t"]) @?= Right (BooleanValue False)
      eval (Call (Symbol "xor") [Symbol "#t", Symbol "#t"]) @?= Right (BooleanValue False)
      eval (Call (Symbol "xor") [Symbol "#f", Symbol "#f"]) @?= Right (BooleanValue False)
      eval (Call (Symbol "xor") [Symbol "#t", Symbol "#f"]) @?= Right (BooleanValue True)
      -- NOTE: division by zero crashes
      --       division with multiple arguments too
      --       odd arguments of (-) are added instead of subtracted
  ]
