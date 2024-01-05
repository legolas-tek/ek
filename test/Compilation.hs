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
    , "test show Bytecode 1" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ Push (IntegerValue 42)
                                                   , Ret
                                                   ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tpush 42\n\tret\n"
    , "test show Bytecode Push" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ Push (IntegerValue 42) ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tpush 42\n"
    , "test show Bytecode Call" ~: do
        let insts = fromList [("foo (a) (b) (c)", [VirtualMachine.Call ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tcall\n"
    , "test show Bytecode CallOp" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ CallOp Add ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tcall_op Add\n"
    , "test show Bytecode JmpFalse" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ JmpFalse 42 ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tjmp_false 42\n"
    , "test show Bytecode Dup" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ Dup ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tdup\n"
    , "test show Bytecode Ret" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ Ret ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tret\n"
    , "test show Bytecode LoadArg" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ LoadArg 42 ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tload_arg 42\n"
    , "test show Bytecode GetEnv" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ GetEnv "foo" ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tgetenv foo\n"
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
