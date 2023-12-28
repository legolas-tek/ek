{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Compiler tests
--}

module Compilation (tests) where

import Test.HUnit

import EK.Ast
import EK.Compiler
import VirtualMachine

tests :: Test
tests = test
  [ "func def" ~: do
      let stmts =
            [ FuncDef
                (FuncPattern
                   [ SymbolPattern "foo"
                   , ArgPattern "a" Nothing
                   , ArgPattern "b" Nothing
                   , ArgPattern "c" Nothing
                   ]
                   Nothing)
                (IntegerLit 42)
            ]
      let expected =
            [ PushEnv "a"
            , PushEnv "b"
            , PushEnv "c"
            , Push (IntegerValue 42)
            , Ret
            ]
      compileToVM stmts @?= Right expected
    , "func def with type" ~: do
        let stmts =
              [ FuncDef
                  (FuncPattern
                     [ SymbolPattern "foo"
                     , ArgPattern "a" (Just $ TypeName "int")
                     , ArgPattern "b" (Just $ TypeName "int")
                     , ArgPattern "c" (Just $ TypeName "int")
                     ]
                     (Just $ TypeName "int"))
                  (IntegerLit 42)
              ]
        let expected =
              [ PushEnv "a"
              , PushEnv "b"
              , PushEnv "c"
              , Push (IntegerValue 42)
              , Ret
              ]
        compileToVM stmts @?= Right expected
    , "call with expressions" ~: do
        let stmts =
              [ FuncDef
                  (FuncPattern
                     [ SymbolPattern "foo"
                     , ArgPattern "a" Nothing
                     , ArgPattern "b" Nothing
                     , ArgPattern "c" Nothing
                     ]
                     Nothing)
                  (EK.Ast.Call (FunctionName [Symbol "foo"]) [ExprCall (IntegerLit 1), ExprCall (StringLit "hello"), ExprCall (IntegerLit 42)])
              ]
        let expected =
              [ PushEnv "a"
              , PushEnv "b"
              , PushEnv "c"
              , Push (IntegerValue 1)
              , Push (StringValue "hello")
              , Push (IntegerValue 42)
              , VirtualMachine.Call
              , Ret
              ]
        compileToVM stmts @?= Right expected
  ]
