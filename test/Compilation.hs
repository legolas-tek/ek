{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Compiler tests
--}

{-# LANGUAGE OverloadedStrings #-}

module Compilation (tests) where

import Test.HUnit

import EK.Ast
import EK.Compiler
import VirtualMachine
import Data.Map (fromList)

tests :: Test
tests = test
  [ "func def" ~: do
      let stmts =
            [ FuncDef
                (FuncPattern
                   [ SymbolPattern "foo"
                   , ArgPattern False "a" Nothing
                   , ArgPattern False "b" Nothing
                   , ArgPattern False "c" Nothing
                   ]
                   Nothing
                   Nothing)
                (IntegerLit 42)
            ]
      let expected = fromList [("foo (a) (b) (c)", [ Push (IntegerValue 42)
                                                   , Ret
                                                   ])]
      compileToVM stmts @?= Right expected
    , "test" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ Push (IntegerValue 42)
                                                   , Ret
                                                   ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tpush 42\n\tret\n"
    , "call with expressions" ~: do
        let stmts =
              [ FuncDef
                  (FuncPattern
                     [ SymbolPattern "foo"
                     , ArgPattern False "a" Nothing
                     , ArgPattern False "b" Nothing
                     , ArgPattern False "c" Nothing
                     ]
                     Nothing
                     Nothing)
                  (EK.Ast.Call (FunctionName [Symbol "foo"] defaultPrec)
                               [IntegerLit 1
                               , StringLit "hello"
                               , IntegerLit 42
                               ]
                  )
              ]
        let expected = fromList [("foo (a) (b) (c)", [ GetEnv "foo"
                                                     , Push (IntegerValue 1)
                                                     , VirtualMachine.Call
                                                     , Push (StringValue "hello")
                                                     , VirtualMachine.Call
                                                     , Push (IntegerValue 42)
                                                     , VirtualMachine.Call
                                                     , Ret
                                                     ])]
        compileToVM stmts @?= Right expected
    , "test a function with args" ~: do
        let stmts =
              [ FuncDef
                  (FuncPattern
                     [ SymbolPattern "id"
                     , ArgPattern False "a" Nothing
                     ]
                     Nothing
                     Nothing)
                  (EK.Ast.Call "a" [])
              ]
        let expected = fromList [("id (a)", [ LoadArg 0
                                            , Ret
                                            ])]
        compileToVM stmts @?= Right expected
  ]
