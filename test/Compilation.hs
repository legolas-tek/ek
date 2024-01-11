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
  [ "show Bytecode Push" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ Push (IntegerValue 42) ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tpush 42\n"
    , "show Bytecode Call" ~: do
        let insts = fromList [("foo (a) (b) (c)", [VirtualMachine.Call ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tcall\n"
    , "show Bytecode CallOp" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ CallOp Add ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tcall_op add\n"
    , "show Bytecode JmpFalse" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ JmpFalse 42 ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tjmp_false 42\n"
    , "show Bytecode Dup" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ Dup ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tdup\n"
    , "show Bytecode Ret" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ Ret ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tret\n"
    , "show Bytecode LoadArg" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ LoadArg 42 ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tload_arg 42\n"
    , "show Bytecode GetEnv" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ GetEnv "foo" ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tgetenv foo\n"
    , "show Bytecode 2" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ Push (IntegerValue 42)
                                                   , Ret
                                                   ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tpush 42\n\tret\n"
    , "show Bytecode 3" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ Push (IntegerValue 42)
                                                   , Ret
                                                   , GetEnv "foo"
                                                   ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tpush 42\n\tret\n\tgetenv foo\n"
    , "show Bytecode 4 overlapping" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ Push (IntegerValue 42)
                                                   , Ret
                                                   , GetEnv "foo"
                                                   , Push (IntegerValue 42)
                                                   ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tpush 42\n\tret\n\tgetenv foo\n\tpush 42\n"
    , "show Bytecode 4 no overlapping" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ Push (IntegerValue 42)
                                                   , Ret
                                                   , GetEnv "foo"
                                                   , LoadArg 42
                                                   ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tpush 42\n\tret\n\tgetenv foo\n\tload_arg 42\n"
    , "show Bytecode 5" ~: do
        let insts = fromList [("foo (a) (b) (c)", [ Push (IntegerValue 42)
                                                   , Ret
                                                   , GetEnv "foo"
                                                   , LoadArg 42
                                                   , CallOp Add
                                                   ])]
        showBytecode insts @?= "foo (a) (b) (c):\n\tpush 42\n\tret\n\tgetenv foo\n\tload_arg 42\n\tcall_op add\n"
    , "constant func with 3 args" ~: do
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
      let expected = fromList [ ("foo _ _ _",
                                 [ GetEnv "foo _ _ _\\a"
                                 , Closure 0 -- could be removed
                                 , Ret
                                 ])
                              , ("foo _ _ _\\a",
                                  [ GetEnv "foo _ _ _\\a\\b"
                                  , Closure 0 -- could be removed
                                  , Ret
                                  ])
                              , ("foo _ _ _\\a\\b",
                                  [ Push $ IntegerValue 42
                                  , Ret
                                  ])
                              ]
      compileToVM stmts @?= Right expected
    , "calling function with 3 args" ~: do
        let stmts =
              [ ExternDef
                  (FuncPattern
                     [ SymbolPattern "foo"
                     , ArgPattern False "a" Nothing
                     , ArgPattern False "b" Nothing
                     , ArgPattern False "c" Nothing
                     ]
                     Nothing
                     Nothing)
              , FuncDef
                  (FuncPattern [SymbolPattern "main"] Nothing Nothing)
                  (EK.Ast.Call (FunctionName [Symbol "foo"] defaultPrec)
                               [ IntegerLit 1
                               , StringLit "hello"
                               , IntegerLit 42
                               ]
                  )
              ]
        let expected = fromList [("main",
                                  [ GetEnv "foo"
                                  , Push (IntegerValue 1)
                                  , VirtualMachine.Call
                                  , Push (StringValue "hello")
                                  , VirtualMachine.Call
                                  , Push (IntegerValue 42)
                                  , VirtualMachine.Call
                                  , Ret
                                  ])]
        compileToVM stmts @?= Right expected
    , "identity function" ~: do
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
        let expected = fromList [("id _",
                                  [ LoadArg 0
                                  , Ret
                                  ])]
        compileToVM stmts @?= Right expected
    , "calling constant" ~: do
        let stmts =
              [ FuncDef
                  (FuncPattern [SymbolPattern "key"] Nothing Nothing)
                  (IntegerLit 42)
              , FuncDef
                  (FuncPattern [SymbolPattern "answer"] Nothing Nothing)
                  (EK.Ast.Call "key" [])
              ]
        let expected = fromList [("key",
                                  [ Push $ IntegerValue 42
                                  , Ret
                                  ])
                                , ("answer",
                                  [ GetEnv "key"
                                  , Push $ AtomValue "void"
                                  , VirtualMachine.Call
                                  , Ret
                                  ])
                                ]
        compileToVM stmts @?= Right expected
    , "lambda closure" ~: do
        let stmts =
              [ FuncDef
                  (FuncPattern
                      [ SymbolPattern "const"
                      , ArgPattern False "a" Nothing
                      ]
                      Nothing
                      Nothing)
                  (Lambda "b"
                     (EK.Ast.Call "a" []))
              ]
        let expected = fromList [("const _",
                                  [ LoadArg 0
                                  , GetEnv "const _\\b0"
                                  , Closure 1
                                  , Ret
                                  ])
                                , ("const _\\b0",
                                  [ LoadArg 1
                                  , Ret
                                  ])
                                ]
        compileToVM stmts @?= Right expected
    , "simple placeholder pattern" ~: do
        let stmts =
              [ FuncDef
                  (FuncPattern
                      [ SymbolPattern "id"
                      , PlaceholderPattern
                      ]
                      Nothing
                      Nothing)
                  (Lambda "a"
                     (EK.Ast.Call "a" []))
              ]
        let expected = fromList [("id _",
                                  [ GetEnv "id _\\a0"
                                  , Closure 0
                                  , LoadArg 0
                                  , VirtualMachine.Call
                                  , Ret
                                  ])
                                , ("id _\\a0",
                                  [ LoadArg 0
                                  , Ret
                                  ])
                                ]
        compileToVM stmts @?= Right expected
    , "3 placeholder patterns" ~: do
        let stmts =
              [ FuncDef
                  (FuncPattern
                      [ SymbolPattern "foo"
                      , PlaceholderPattern
                      , PlaceholderPattern
                      , PlaceholderPattern
                      ]
                      Nothing
                      Nothing)
                  (EK.Ast.Call "aliased" [])
              , ExternDef
                  (FuncPattern [SymbolPattern "aliased"] Nothing Nothing)
              ]
        let expected = fromList [("foo _ _ _",
                                  [ LoadArg 0
                                  , GetEnv "foo _ _ _\\_1"
                                  , Closure 1
                                  , Ret
                                  ])
                                , ("foo _ _ _\\_1",
                                  [ LoadArg 0
                                  , LoadArg 1
                                  , GetEnv "foo _ _ _\\_1\\_2"
                                  , Closure 2
                                  , Ret
                                  ])
                                , ("foo _ _ _\\_1\\_2",
                                  [ GetEnv "aliased"
                                  , Push $ AtomValue "void"
                                  , VirtualMachine.Call
                                  , LoadArg 1
                                  , VirtualMachine.Call
                                  , LoadArg 2
                                  , VirtualMachine.Call
                                  , LoadArg 0
                                  , VirtualMachine.Call
                                  , Ret
                                  ])
                                ]
        compileToVM stmts @?= Right expected
    , "lazy arg" ~: do
        let stmts =
              [ FuncDef
                  (FuncPattern [SymbolPattern "do", ArgPattern True "a" Nothing]
                      Nothing
                      Nothing)
                  (EK.Ast.Call "a" [])
              ]
        let expected = fromList [("do _",
                                  [ LoadArg 0
                                  , Push $ AtomValue "void"
                                  , VirtualMachine.Call
                                  , Ret
                                  ])
                                ]
        compileToVM stmts @?= Right expected
    , "Atoms" ~: do
        let stmts = [AtomDef "foo"]
        let expected = fromList [("foo",
                                  [ Push $ AtomValue "foo"
                                  , Ret
                                  ])
                                ]
        compileToVM stmts @?= Right expected
    , "Struct" ~: do
        let stmts = [StructDef "foo" [ StructElem "a" (TypeName "int")
                                     , StructElem "b" (TypeName "int")]]
        let expected = fromList [("_ a",
                                  [ LoadArg 0
                                  , Extract 0
                                  , Ret
                                  ])
                                , ("_ b",
                                  [ LoadArg 0
                                  , Extract 1
                                  , Ret
                                  ])
                                ]
        compileToVM stmts @?= Right expected
    , "Construct" ~: do
        let stmts = [FuncDef (FuncPattern [SymbolPattern "myfoo"] Nothing Nothing)
                              (StructLit (TypeName "foo")
                               [IntegerLit 1, IntegerLit 2])]
        let expected = fromList [("myfoo",
                                  [ Push $ IntegerValue 1
                                  , Push $ IntegerValue 2
                                  , Construct "foo" 2
                                  , Ret
                                  ])
                                ]
        compileToVM stmts @?= Right expected
  ]
