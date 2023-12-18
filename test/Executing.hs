{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

module Executing (tests) where

import Test.HUnit

import VirtualMachine

tests :: Test
tests = test
  [ "const" ~: do
      exec [Push $ IntegerValue 32, Ret] [] @?= Right [IntegerValue 32]
      exec [Push $ IntegerValue 10, Push $ IntegerValue 52, CallOp Sub, Ret] [] @?= Right [IntegerValue 42]
  , "errorHandling" ~: do
      exec [Push $ IntegerValue 10, CallOp Add, Ret] [] @?= Left "Not enough arguments for operator"
      exec [Push $ IntegerValue 10, Push $ BooleanValue True, CallOp Add, Ret] [] @?= Left "Invalid operands for operator"
      exec [Push $ IntegerValue 0, Push $ IntegerValue 10, CallOp Div, Ret] [] @?= Left "Division by zero"
  ]
