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

import Control.Exception

handler :: IOError -> IO VMValue
handler e = return $ StringValue (show e)

catchExec :: [Instruction] -> IO VMValue
catchExec instructions = catch (ex instructions []) handler

ex :: Insts -> Stack -> IO VMValue
ex = exec empty []

absFn :: [Instruction]
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
      simpleIntRet <- ex [Push $ IntegerValue 42, Ret] []
      simpleIntRet @?= IntegerValue 42
      subOp <- ex [Push $ IntegerValue 10, Push $ IntegerValue 52, CallOp Sub, Ret] []
      subOp @?= IntegerValue 42
      simpleStrRet <- ex [Push $ StringValue "Hello", Ret] []
      simpleStrRet @?= StringValue "Hello"
  , "errorHandling" ~: do
      errorAddOp <- catchExec [Push $ IntegerValue 10, CallOp Add, Call, Ret]
      errorAddOp @?= StringValue "user error (Not enough arguments for operator)"
      invalidArgs <- catchExec [Push $ IntegerValue 10, Push $ AtomValue "true", CallOp Add, Call, Ret]
      invalidArgs @?= StringValue "user error (Invalid operands for operator)"
      divZero <- catchExec [Push $ IntegerValue 0, Push $ IntegerValue 10, CallOp Div, Call, Ret]
      divZero @?= StringValue "user error (Division by zero)"
  , "comparison" ~: do
      eqTrue <- ex [ Push $ IntegerValue 10
                , Push $ IntegerValue 10
                , CallOp Eq
                , Ret
                ] []
      eqTrue @?= AtomValue "true"
      eqFalse <- ex [ Push $ IntegerValue 10
                    , Push $ IntegerValue 11
                    , CallOp Eq
                    , Ret
                    ] []
      eqFalse @?= AtomValue "false"
      lessFalse <- ex [Push $ IntegerValue 2
                      , Push $ IntegerValue 5
                      , CallOp Less
                      , Ret] []
      lessFalse @?= AtomValue "false"
      lessTrue <- ex [Push $ IntegerValue 5
                     , Push $ IntegerValue 2
                     , CallOp Less
                     , Ret] []
      lessTrue @?= AtomValue "true"
  , "conditionalJump" ~: do
      let conditionalJump v =
            [ Push $ AtomValue v
            , JmpFalse 2
            , Push $ IntegerValue 1
            , Ret
            , Push $ IntegerValue 2
            , Ret
            ]
      true <- ex (conditionalJump "true") []
      false <- ex (conditionalJump "false") []
      true @?= IntegerValue 1
      false @?= IntegerValue 2
  , "function" ~: do
      negativeAbsFn <- ex [ Push $ FunctionValue absFn
                          , Push $ IntegerValue (-42)
                          , Call
                          , Ret
                          ] []
      negativeAbsFn @?= IntegerValue 42
      positiveAbsFn <- ex [ Push $ FunctionValue absFn
                          , Push $ IntegerValue 42
                          , Call
                          , Ret
                          ] []
      positiveAbsFn @?= IntegerValue 42
    , "GetEnv" ~: do
        result <- exec (fromList [("a", IntegerValue 42)]) [] [GetEnv "a", Ret] []
        result @?= IntegerValue 42
        result2 <- catchExec [GetEnv "failure expected"]
        result2 @?= StringValue "user error (Could not find `failure expected' in environment)"
    , "operators" ~: do
        addInts <- ex [Push $ IntegerValue 10, Push $ IntegerValue 5, CallOp Add, Ret] []
        addInts @?= IntegerValue 15
        addFloats <- ex [Push $ FloatValue 10.5, Push $ FloatValue 5.3, CallOp Add, Ret] []
        addFloats @?= FloatValue 15.8
        addIntFloat <- ex [Push $ IntegerValue 10, Push $ FloatValue 5.3, CallOp Add, Ret] []
        addIntFloat @?= FloatValue 15.3
        addFloatInt <- ex [Push $ FloatValue 10.5, Push $ IntegerValue 5, CallOp Add, Ret] []
        addFloatInt @?= FloatValue 15.5
        subInts <- ex [Push $ IntegerValue 10, Push $ IntegerValue 5, CallOp Sub, Ret] []
        subInts @?= IntegerValue (-5)
        subFloats <- ex [Push $ FloatValue 10.5, Push $ FloatValue 5.3, CallOp Sub, Ret] []
        subFloats @?= FloatValue (-5.2)
        subFloatInt <- ex [Push $ FloatValue 10.5, Push $ IntegerValue 5, CallOp Sub, Ret] []
        subFloatInt @?= FloatValue (-5.5)
        subIntFloat <- ex [Push $ IntegerValue 10, Push $ FloatValue 5.5, CallOp Sub, Ret] []
        subIntFloat @?= FloatValue (-4.5)
        mulInts <- ex [Push $ IntegerValue 10, Push $ IntegerValue 5, CallOp Mul, Ret] []
        mulInts @?= IntegerValue 50
        mulFloats <- ex [Push $ FloatValue 1.5, Push $ FloatValue 2.5, CallOp Mul, Ret] []
        mulFloats @?= FloatValue 3.75
        mulFloatInt <- ex [Push $ FloatValue 1.5, Push $ IntegerValue 2, CallOp Mul, Ret] []
        mulFloatInt @?= FloatValue 3
        mulIntFloat <- ex [Push $ IntegerValue 3, Push $ FloatValue 2.5, CallOp Mul, Ret] []
        mulIntFloat @?= FloatValue 7.5
        division <- ex [Push $ IntegerValue 5, Push $ IntegerValue 10, CallOp Div, Ret] []
        division @?= IntegerValue 2
        printOp <- catchExec [Push $ IntegerValue 42, CallOp Print, Ret]
        printOp @?= StringValue "user error (No value on stack)"
        equalityIntsFalse <- ex [Push $ IntegerValue 5, Push $ IntegerValue 10, CallOp Eq, Ret] []
        equalityIntsFalse @?= AtomValue "false"
        equalityIntsTrue <- ex [Push $ IntegerValue 5, Push $ IntegerValue 5, CallOp Eq, Ret] []
        equalityIntsTrue @?= AtomValue "true"
        equalityFloatsTrue <- ex [Push $ FloatValue 5.5, Push $ FloatValue 5.5, CallOp Eq, Ret] []
        equalityFloatsTrue @?= AtomValue "true"
        lessIntsFalse <- ex [Push $ IntegerValue 5, Push $ IntegerValue 10, CallOp Less, Ret] []
        lessIntsFalse @?= AtomValue "false"
        lessIntsTrue <- ex [Push $ IntegerValue 7, Push $ IntegerValue 5, CallOp Less, Ret] []
        lessIntsTrue @?= AtomValue "true"
        lessFloatsTrue <- ex [Push $ FloatValue 5.5, Push $ FloatValue 5.3, CallOp Less, Ret] []
        lessFloatsTrue @?= AtomValue "true"
        concatOp <- ex [Push $ StringValue "World", Push $ StringValue "Hello", CallOp Concat, Ret] []
        concatOp @?= StringValue "HelloWorld"
        concatIntOp <- ex [Push $ IntegerValue 5, Push $ IntegerValue 5, CallOp Concat, Ret] []
        concatIntOp @?= IntegerValue 55
        concatFloatOp <- ex [Push $ FloatValue 5.5, Push $ IntegerValue 5, CallOp Concat, Ret] []
        concatFloatOp @?= FloatValue 55.5
        concatFloatOp2 <- ex [Push $ IntegerValue 5, Push $ FloatValue 5.5, CallOp Concat, Ret] []
        concatFloatOp2 @?= FloatValue 5.55
    , "closures" ~: do
        let addition = [ LoadArg 0
                       , LoadArg 1
                       , CallOp Add
                       , Ret
                       ]
        let closure = [ Push $ IntegerValue 10
                      , Push $ FunctionValue addition
                      , Closure 1
                      , Ret
                      ]
        let usage = [ Push $ FunctionValue closure
                    , Push $ AtomValue "void"
                    , Call
                    , Push $ IntegerValue 32
                    , Call
                    , Ret
                    ]
        result <- ex usage []
        result @?= IntegerValue 42
  ]
