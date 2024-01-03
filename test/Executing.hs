{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

module Executing (tests) where

import Test.HUnit

import VirtualMachine

import Data.Map (fromList, empty)

ex = exec empty []

absFn = [ LoadArg 0
        , Push $ IntegerValue 0
        , CallOp Less
        , JmpFalse 2
        , LoadArg 0
        , Ret
        , LoadArg 0
        , Push $ IntegerValue (-1)
        , CallOp Mul
        , Ret
        ]

tests :: Test
tests = test
  [ "const" ~: do
      ex [Push $ IntegerValue 32, Ret] [] @?= Right (IntegerValue 32)
      ex [Push $ IntegerValue 10, Push $ IntegerValue 52, CallOp Sub, Ret] [] @?= Right (IntegerValue 42)
      ex [Push $ StringValue "Hello", Ret] [] @?= Right (StringValue "Hello")
  , "errorHandling" ~: do
      ex [Push $ IntegerValue 10, CallOp Add, Call, Ret] [] @?= Left "Not enough arguments for operator"
      ex [Push $ IntegerValue 10, Push $ AtomValue "true", CallOp Add, Call, Ret] [] @?= Left "Invalid operands for operator"
      ex [Push $ IntegerValue 0, Push $ IntegerValue 10, CallOp Div, Call, Ret] [] @?= Left "Division by zero"
  , "comparison" ~: do
      ex [ Push $ IntegerValue 10
         , Push $ IntegerValue 10
         , CallOp Eq
         , Ret
         ] [] @?= Right (AtomValue "true")
      ex [ Push $ IntegerValue 10
           , Push $ IntegerValue 11
           , CallOp Eq
           , Ret
           ] [] @?= Right (AtomValue "false")
      ex [Push $ IntegerValue 2
         , Push $ IntegerValue 5
         , CallOp Less
         , Ret] [] @?= Right (AtomValue "false")
      ex [Push $ IntegerValue 5
         , Push $ IntegerValue 2
         , CallOp Less
         , Ret] [] @?= Right (AtomValue "true")
  , "conditionalJump" ~: do
      let conditionalJump v =
            [ Push $ AtomValue v
            , JmpFalse 2
            , Push $ IntegerValue 1
            , Ret
            , Push $ IntegerValue 2
            , Ret
            ]
      ex (conditionalJump "true") [] @?= Right (IntegerValue 1)
      ex (conditionalJump "false") [] @?= Right (IntegerValue 2)
  , "function" ~: do
      ex [ Push $ FunctionValue absFn
         , Push $ IntegerValue (-42)
         , Call
         , Ret
         ] [] @?= Right (IntegerValue 42)
      ex [ Push $ FunctionValue absFn
         , Push $ IntegerValue 20
         , Call
         , Ret
         ] [] @?= Right (IntegerValue 20)
    , "PopEnv" ~: do
        exec (fromList [("a", IntegerValue 42)]) [] [PopEnv "a", Ret] [] @?= Right (IntegerValue 42)
        ex [PopEnv "failure expected"] [] @?=
          Left "No value to pop from env"
  ]
