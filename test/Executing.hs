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
      simpleIntRet @?= (IntegerValue 42)
      subOp <- ex [Push $ IntegerValue 10, Push $ IntegerValue 52, CallOp Sub, Ret] []
      subOp @?= (IntegerValue 42)
      simpleStrRet <- ex [Push $ StringValue "Hello", Ret] []
      simpleStrRet @?= (StringValue "Hello")
  , "errorHandling" ~: do
      errorAddOp <- catch (ex [Push $ IntegerValue 10, CallOp Add, Call, Ret] []) handler
      errorAddOp @?= StringValue "user error (Not enough arguments for operator)"
      invalidArgs <- catch (ex [Push $ IntegerValue 10, Push $ AtomValue "true", CallOp Add, Call, Ret] []) handler
      invalidArgs @?= StringValue "user error (Invalid operands for operator)"
      divZero <- catch (ex [Push $ IntegerValue 0, Push $ IntegerValue 10, CallOp Div, Call, Ret] []) handler
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
        result2 <- catch (ex [GetEnv "failure expected"] []) handler 
        result2 @?= StringValue "user error (No value in env)"
    , "operators" ~: do
        add <- ex [Push $ IntegerValue 10, Push $ IntegerValue 5, CallOp Add, Ret] []
        add @?= (IntegerValue 15)
        sub <- ex [Push $ IntegerValue 10, Push $ IntegerValue 5, CallOp Sub, Ret] []
        sub @?= (IntegerValue (-5))
        mul <- ex [Push $ IntegerValue 10, Push $ IntegerValue 5, CallOp Mul, Ret] []
        mul @?= (IntegerValue 50)
        division <- ex [Push $ IntegerValue 5, Push $ IntegerValue 10, CallOp Div, Ret] []
        division @?= (IntegerValue 2)
        printOp <- catch (ex [Push $ IntegerValue 42, CallOp Print, Ret] []) handler
        printOp @?= StringValue "user error (No value on stack)"
  ]
