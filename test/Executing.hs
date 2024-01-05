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

ex :: Insts -> Stack -> IO (Either String VMValue)
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
      simpleIntRet @?= Right (IntegerValue 42)
      subOp <- ex [Push $ IntegerValue 10, Push $ IntegerValue 52, CallOp Sub, Ret] []
      subOp @?= Right (IntegerValue 42)
      simpleStrRet <- ex [Push $ StringValue "Hello", Ret] []
      simpleStrRet @?= Right (StringValue "Hello")
  , "errorHandling" ~: do
      errorAddOp <- ex [Push $ IntegerValue 10, CallOp Add, Call, Ret] []
      errorAddOp @?= Left "Not enough arguments for operator"
      invalidArgs <- ex [Push $ IntegerValue 10, Push $ AtomValue "true", CallOp Add, Call, Ret] []
      invalidArgs @?= Left "Invalid operands for operator"
      divZero <- ex [Push $ IntegerValue 0, Push $ IntegerValue 10, CallOp Div, Call, Ret] []
      divZero @?= Left "Division by zero"
  , "comparison" ~: do
      eqTrue <- ex [ Push $ IntegerValue 10
                , Push $ IntegerValue 10
                , CallOp Eq
                , Ret
                ] []
      eqTrue @?= Right (AtomValue "true")
      eqFalse <- ex [ Push $ IntegerValue 10
                    , Push $ IntegerValue 11
                    , CallOp Eq
                    , Ret
                    ] []
      eqFalse @?= Right (AtomValue "false")
      lessFalse <- ex [Push $ IntegerValue 2
                      , Push $ IntegerValue 5
                      , CallOp Less
                      , Ret] []
      lessFalse @?= Right (AtomValue "false")
      lessTrue <- ex [Push $ IntegerValue 5
                     , Push $ IntegerValue 2
                     , CallOp Less
                     , Ret] []
      lessTrue @?= Right (AtomValue "true")
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
      true @?= Right (IntegerValue 1)
      false @?= Right (IntegerValue 2)
  , "function" ~: do
      negativeAbsFn <- ex [ Push $ FunctionValue absFn
                          , Push $ IntegerValue (-42)
                          , Call
                          , Ret
                          ] []
      negativeAbsFn @?= Right (IntegerValue 42)
      positiveAbsFn <- ex [ Push $ FunctionValue absFn
                          , Push $ IntegerValue 42
                          , Call
                          , Ret
                          ] []
      positiveAbsFn @?= Right (IntegerValue 42)
    , "GetEnv" ~: do
        result <- exec (fromList [("a", IntegerValue 42)]) [] [GetEnv "a", Ret] []
        result @?= Right (IntegerValue 42)
        result2 <- ex [GetEnv "failure expected"] [] 
        result2 @?= Left "No value in env"
    , "operators" ~: do
        add <- ex [Push $ IntegerValue 10, Push $ IntegerValue 5, CallOp Add, Ret] []
        add @?= Right (IntegerValue 15)
        sub <- ex [Push $ IntegerValue 10, Push $ IntegerValue 5, CallOp Sub, Ret] []
        sub @?= Right (IntegerValue (-5))
        mul <- ex [Push $ IntegerValue 10, Push $ IntegerValue 5, CallOp Mul, Ret] []
        mul @?= Right (IntegerValue 50)
        division <- ex [Push $ IntegerValue 5, Push $ IntegerValue 10, CallOp Div, Ret] []
        division @?= Right (IntegerValue 2)
        printOp <- ex [Push $ IntegerValue 42, CallOp Print, Ret] []
        printOp @?= Left "No value on stack"
  ]
