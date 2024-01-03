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

tests :: Test
tests = test
  [ "const" ~: do
      exec empty [Push $ IntegerValue 32, Ret] [] @?= Right [IntegerValue 32]
      exec empty [Push $ IntegerValue 10, Push $ IntegerValue 52, Push $ OperatorValue Sub, Call, Ret] [] @?= Right [IntegerValue 42]
      exec empty [Push $ StringValue "Hello", Ret] [] @?= Right [StringValue "Hello"]
  , "errorHandling" ~: do
      exec empty [Push $ IntegerValue 10, Push $ OperatorValue Add, Call, Ret] [] @?= Left "Not enough arguments for operator"
      exec empty [Push $ IntegerValue 10, Push $ AtomValue "true", Push $ OperatorValue Add, Call, Ret] [] @?= Left "Invalid operands for operator"
      exec empty [Push $ IntegerValue 0, Push $ IntegerValue 10, Push $ OperatorValue Div, Call, Ret] [] @?= Left "Division by zero"
  , "comparison" ~: do
      exec empty [ Push $ IntegerValue 10
           , Push $ IntegerValue 10
           , Push $ OperatorValue Eq
           , Call, Ret
           ] [] @?= Right [AtomValue "true"]
      exec empty [ Push $ IntegerValue 10
           , Push $ IntegerValue 11
           , Push $ OperatorValue Eq
           , Call, Ret
           ] [] @?= Right [AtomValue "false"]
      exec empty [Push $ IntegerValue 2, Push $ IntegerValue 5, Push $ OperatorValue Less, Call, Ret] [] @?= Right [AtomValue "false"]
      exec empty [Push $ IntegerValue 5, Push $ IntegerValue 2, Push $ OperatorValue Less, Call, Ret] [] @?= Right [AtomValue "true"]
  , "conditionalJump" ~: do
      let conditionalJump v =
            [ Push $ AtomValue v
            , JmpFalse 2
            , Push $ IntegerValue 1
            , Ret
            , Push $ IntegerValue 2
            , Ret
            ]
      exec empty (conditionalJump "true") [] @?= Right [IntegerValue 1]
      exec empty (conditionalJump "false") [] @?= Right [IntegerValue 2]
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
      exec empty [ Push $ IntegerValue (-42)
           , Push $ FunctionValue absFn
           , Call
           , Ret
           ] [] @?= Right [IntegerValue 42]
      exec empty [ Push $ IntegerValue 20
           , Push $ FunctionValue absFn
           , Call
           , Ret
           ] [] @?= Right [IntegerValue 20]
   ,"env" ~: do
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
      exec (fromList [("a", IntegerValue 42)]) [PushEnv "a"] [] @?= Right [IntegerValue 42]
      exec (fromList [("absFn", FunctionValue absFn)]) [ Push $ IntegerValue (-42)
           , PushEnv "absFn"
           , Call
           , Ret
           ] [] @?= Right [IntegerValue 42]
      exec empty [PushEnv "failure expected"] [] @?=
        Left "Couldn't find requested VMValue in env"
    , "PopEnv" ~: do
        exec (fromList [("a", IntegerValue 42)]) [PopEnv "a", PushEnv "a"] [] @?= Left "Couldn't find requested VMValue in env"
        exec empty [PopEnv "failure expected"] [] @?=
          Left "No value to pop from env"
  ]
