{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

module EKAstShowing (tests) where

import Test.HUnit

import EK.Ast

show' :: Stmt Expr -> String
show' = show

tests :: Test
tests = test
  [ "expr" ~: do
      show (IntegerLit 42) @?= "42"
      show (StringLit "foo") @?= "\"foo\""
      show (Call (FunctionName [Symbol "foo"]) []) @?= "foo"
      show (Call (FunctionName [Symbol "foo", Placeholder]) [ExprCall (IntegerLit 42)]) @?= "foo (42)"
      show (Call (FunctionName [Symbol "foo", Placeholder]) [PlaceholderCall]) @?= "foo _"
      show (Call (FunctionName [Symbol "foo", Placeholder, Symbol "bar"]) [ExprCall (IntegerLit 42)]) @?= "foo (42) bar"
      show (Call (FunctionName [Placeholder, Symbol "+", Placeholder]) [PlaceholderCall, ExprCall (IntegerLit 1)]) @?= "_ + (1)"
  , "type def" ~: do
      show' (AtomDef "foo") @?= "atom foo"
      show' (TypeDef "foo" (TypeName "bar")) @?= "type foo = bar"
      show' (TypeDef "foo" (IntRange (Just 0) (Just 42))) @?= "type foo = [0..42]"
      show' (TypeDef "uint" (IntRange (Just 0) Nothing)) @?= "type uint = [0..]"
      show' (TypeDef "negative" (IntRange Nothing (Just 0))) @?= "type negative = [..0]"
      show' (TypeDef "foo" (UnionType (TypeName "bar") (TypeName "baz"))) @?= "type foo = bar | baz"
      show' (StructDef "foo" []) @?= "struct foo {}"
      show' (StructDef "foo" [StructElem "bar" (TypeName "baz")]) @?= "struct foo { bar : baz }"
      show' (StructDef "foo" [StructElem "bar" (TypeName "baz"), StructElem "code" (IntRange Nothing Nothing)]) @?= "struct foo { bar : baz, code : [..] }"
  , "func def" ~: do
      show' (FuncDef (FuncPattern [SymbolPattern "foo"] Nothing) (IntegerLit 42)) @?= "fn foo = 42"
      show' (FuncDef (FuncPattern [SymbolPattern "foo"] (Just (TypeName "bar"))) (IntegerLit 42)) @?= "fn foo : bar = 42"
      show' (FuncDef (FuncPattern [SymbolPattern "foo", SymbolPattern "bar"] Nothing) (IntegerLit 42)) @?= "fn foo bar = 42"
      show' (FuncDef (FuncPattern [ArgPattern "a" Nothing, SymbolPattern "+", ArgPattern "b" Nothing] Nothing) (IntegerLit 42)) @?= "fn (a) + (b) = 42"
  , "extern def" ~: do
      show' (ExternDef (FuncPattern [SymbolPattern "exit", ArgPattern "code" (Just $ TypeName "int")] (Just $ TypeName "never"))) @?= "extern fn exit (code : int) : never"
  , "function names" ~: do
      show (FunctionName [Symbol "foo"]) @?= "foo"
      show (FunctionName [Symbol "foo", Placeholder]) @?= "foo _"
      show (FunctionName [Symbol "foo", Placeholder, Symbol "bar"]) @?= "foo _ bar"
      show (FunctionName [Placeholder, Symbol "+", Placeholder]) @?= "_ + _"
  ]
