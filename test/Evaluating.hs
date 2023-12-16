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
  ]
