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
  , "comparison" ~: do
      exec [Push $ IntegerValue 10, Push $ IntegerValue 10, CallOp Eq, Ret] [] @?= Right [BooleanValue True]
      exec [Push $ IntegerValue 10, Push $ IntegerValue 11, CallOp Eq, Ret] [] @?= Right [BooleanValue False]
      exec [Push $ IntegerValue 2, Push $ IntegerValue 5, CallOp Less, Ret] [] @?= Right [BooleanValue False]
      exec [Push $ IntegerValue 5, Push $ IntegerValue 2, CallOp Less, Ret] [] @?= Right [BooleanValue True]
  , "conditionalJump" ~: do
      let conditionalJump v =
            [ Push $ BooleanValue v
            , JmpFalse 2
            , Push $ IntegerValue 1
            , Ret
            , Push $ IntegerValue 2
            , Ret
            ]
      exec (conditionalJump True) [] @?= Right [IntegerValue 1]
      exec (conditionalJump False) [] @?= Right [IntegerValue 2]
  ]
