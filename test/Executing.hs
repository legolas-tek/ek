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
      exec [] [Push $ IntegerValue 32, Ret] [] @?= Right [IntegerValue 32]
      exec [] [Push $ IntegerValue 10, Push $ IntegerValue 52, Push $ OperatorValue Sub, Call, Ret] [] @?= Right [IntegerValue 42]
  , "errorHandling" ~: do
      exec [] [Push $ IntegerValue 10, Push $ OperatorValue Add, Call, Ret] [] @?= Left "Not enough arguments for operator"
      exec [] [Push $ IntegerValue 10, Push $ BooleanValue True, Push $ OperatorValue Add, Call, Ret] [] @?= Left "Invalid operands for operator"
      exec [] [Push $ IntegerValue 0, Push $ IntegerValue 10, Push $ OperatorValue Div, Call, Ret] [] @?= Left "Division by zero"
  , "comparison" ~: do
      exec [] [ Push $ IntegerValue 10
           , Push $ IntegerValue 10
           , Push $ OperatorValue Eq
           , Call, Ret
           ] [] @?= Right [BooleanValue True]
      exec [] [ Push $ IntegerValue 10
           , Push $ IntegerValue 11
           , Push $ OperatorValue Eq
           , Call, Ret
           ] [] @?= Right [BooleanValue False]
      exec [] [Push $ IntegerValue 2, Push $ IntegerValue 5, Push $ OperatorValue Less, Call, Ret] [] @?= Right [BooleanValue False]
      exec [] [Push $ IntegerValue 5, Push $ IntegerValue 2, Push $ OperatorValue Less, Call, Ret] [] @?= Right [BooleanValue True]
  , "conditionalJump" ~: do
      let conditionalJump v =
            [ Push $ BooleanValue v
            , JmpFalse 2
            , Push $ IntegerValue 1
            , Ret
            , Push $ IntegerValue 2
            , Ret
            ]
      exec [] (conditionalJump True) [] @?= Right [IntegerValue 1]
      exec [] (conditionalJump False) [] @?= Right [IntegerValue 2]
  , "function" ~: do
      let absFn = [ Dup
                  , Push $ IntegerValue 0
                  , Push $ OperatorValue Less
                  , Call
                  , JmpFalse 1
                  , Ret
                  , Push $ IntegerValue (-1)
                  , Push $ OperatorValue Mul
                  , Call
                  , Ret
                  ]
      exec [] [ Push $ IntegerValue (-42)
           , Push $ FunctionValue absFn
           , Call
           , Ret
           ] [] @?= Right [IntegerValue 42]
      exec [] [ Push $ IntegerValue 20
           , Push $ FunctionValue absFn
           , Call
           , Ret
           ] [] @?= Right [IntegerValue 20]
   ,"env" ~: do
      exec [IntegerValue 42] [PushEnv $ IntegerValue 42] [] @?= Right [IntegerValue 42]
  ]
