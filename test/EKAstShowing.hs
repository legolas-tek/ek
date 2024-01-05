{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

{-# LANGUAGE OverloadedStrings #-}

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
      show (Call (FunctionName [Symbol "foo"] defaultPrec) []) @?= "(foo)"
      show (Call (FunctionName [Symbol "foo", Placeholder] defaultPrec) [IntegerLit 42]) @?= "(foo 42)"
      show (Call (FunctionName [Symbol "foo", Placeholder, Symbol "bar"] defaultPrec) [IntegerLit 42]) @?= "(foo 42 bar)"
      show (Call (FunctionName [Placeholder, Symbol "+", Placeholder] defaultPrec) [Call (FunctionName [Symbol "a"] defaultPrec) [], IntegerLit 1]) @?= "((a) + 1)"
  , "type def" ~: do
      show' (AtomDef "foo") @?= "atom foo"
      show' (ImportDef "foo") @?= "import foo"
      show' (TypeDef "foo" (TypeName "bar")) @?= "type foo = bar"
      show' (TypeDef "foo" (IntRange (Just 0) (Just 42))) @?= "type foo = [0..42]"
      show' (TypeDef "uint" (IntRange (Just 0) Nothing)) @?= "type uint = [0..]"
      show' (TypeDef "negative" (IntRange Nothing (Just 0))) @?= "type negative = [..0]"
      show' (TypeDef "foo" (UnionType (TypeName "bar") (TypeName "baz"))) @?= "type foo = bar | baz"
      show' (StructDef "foo" []) @?= "struct foo {}"
      show' (StructDef "foo" [StructElem "bar" (TypeName "baz")]) @?= "struct foo { bar : baz }"
      show' (StructDef "foo" [StructElem "bar" (TypeName "baz"), StructElem "code" (IntRange Nothing Nothing)]) @?= "struct foo { bar : baz, code : [..] }"
  , "func def" ~: do
      show' (FuncDef (FuncPattern [SymbolPattern "foo"] Nothing Nothing) (IntegerLit 42)) @?= "fn foo = 42"
      show' (FuncDef (FuncPattern [SymbolPattern "foo"] (Just (TypeName "bar")) Nothing) (IntegerLit 42)) @?= "fn foo : bar = 42"
      show' (FuncDef (FuncPattern [SymbolPattern "foo", SymbolPattern "bar"] Nothing Nothing) (IntegerLit 42)) @?= "fn foo bar = 42"
      show' (FuncDef (FuncPattern [ArgPattern False "a" Nothing, SymbolPattern "+", ArgPattern False "b" Nothing] Nothing Nothing) (IntegerLit 42)) @?= "fn (a) + (b) = 42"
      show' (FuncDef (FuncPattern [ArgPattern False "a" Nothing, SymbolPattern "+", ArgPattern False "b" Nothing] Nothing (Just 6)) (IntegerLit 42)) @?= "fn (a) + (b) precedence 6 = 42"
      show' (FuncDef (FuncPattern [ArgPattern False "a" Nothing, SymbolPattern "+", ArgPattern False "b" Nothing] (Just $ TypeName "int") (Just 6)) (IntegerLit 42)) @?= "fn (a) + (b) : int precedence 6 = 42"
      show' (FuncDef (FuncPattern [ArgPattern True "a" Nothing, SymbolPattern "+", ArgPattern True "b" Nothing] Nothing Nothing) (IntegerLit 42)) @?= "fn (lazy a) + (lazy b) = 42"
      show' (FuncDef (FuncPattern [ArgPattern True "a" Nothing, SymbolPattern "+", ArgPattern True "b" Nothing] Nothing (Just 6)) (IntegerLit 42)) @?= "fn (lazy a) + (lazy b) precedence 6 = 42"
      show' (FuncDef (FuncPattern [ArgPattern True "a" Nothing, SymbolPattern "+", ArgPattern True "b" Nothing] (Just $ TypeName "int") (Just 6)) (IntegerLit 42)) @?= "fn (lazy a) + (lazy b) : int precedence 6 = 42"
  , "extern def" ~: do
      show' (ExternDef (FuncPattern [SymbolPattern "exit", ArgPattern False "code" (Just $ TypeName "int")] (Just $ TypeName "never") Nothing)) @?= "extern fn exit (code : int) : never"
  , "function names" ~: do
      show (FunctionName [Symbol "foo"] defaultPrec) @?= "foo"
      show (FunctionName [Symbol "foo", Placeholder] defaultPrec) @?= "foo _"
      show (FunctionName [Symbol "foo", Placeholder, Symbol "bar"] defaultPrec) @?= "foo _ bar"
      show (FunctionName [Placeholder, Symbol "+", Placeholder] defaultPrec) @?= "_ + _"
  , "function name overloaded strings" ~: do
      show ("foo" :: FunctionName) @?= "foo"
      show ("foo _" :: FunctionName) @?= "foo _"
      show ("foo _ bar" :: FunctionName) @?= "foo _ bar"
      show ("_ + _" :: FunctionName) @?= "_ + _"
      show ("_ + _" `precedence` 6) @?= "_ + _ precedence 6"
  ]
