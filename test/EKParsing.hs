{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module EKParsing (tests) where

import Test.HUnit

import EK.Parser
import Token
import EK.Ast

tk :: String -> TokenType -> Token
tk s = Token s 0 0 ""

tkt :: TokenType -> Token
tkt = tk ""

idt :: String -> Token
idt s = tk s TextIdentifier

int :: Int -> Token
int i = tk (show i) IntLiter

doc :: [Token] -> Either String [Stmt Expr]
doc t = parseDocument t

tests :: Test
tests = test
  [ "atom" ~: do
      doc [tkt AtomKw, idt "foo"] @?= Right [AtomDef "foo"]
  , "type" ~: do
      doc [tkt TypeKw, idt "foo", tkt Equal, idt "bar"] @?= Right [TypeDef "foo" (TypeName "bar")]
      doc [tkt TypeKw, idt "bit", tkt Equal, tkt BracketOpen, int 0, tkt DotDot, int 1, tkt BracketClose]
        @?= Right [TypeDef "bit" (IntRange (Just 0) (Just 1))]
      doc [tkt TypeKw, idt "bit", tkt Equal, int 0, tkt Pipe, int 1]
        @?= Right [TypeDef "bit" (UnionType (IntRange (Just 0) (Just 0)) (IntRange (Just 1) (Just 1)))]
      doc [tkt TypeKw, idt "int", tkt Equal, tkt BracketOpen, tkt DotDot, tkt BracketClose]
        @?= Right [TypeDef "int" (IntRange Nothing Nothing)]
      doc [tkt TypeKw, idt "uint", tkt Equal, tkt BracketOpen, int 0, tkt DotDot, tkt BracketClose]
        @?= Right [TypeDef "uint" (IntRange (Just 0) Nothing)]
      doc [tkt TypeKw, idt "foo", tkt Equal, tkt BracketOpen, tkt DotDot, int 42, tkt BracketClose]
        @?= Right [TypeDef "foo" (IntRange Nothing (Just 42))]
  , "struct" ~: do
      doc [tkt StructKw, idt "empty", tkt CurlyOpen, tkt CurlyClose] @?= Right [StructDef "empty" []]
      doc [ tkt StructKw, idt "foo", tkt CurlyOpen
          , idt "bar", tkt Colon, idt "baz"
          , tkt CurlyClose]
        @?= Right [StructDef "foo" [StructElem "bar" (TypeName "baz")]]
      doc [ tkt StructKw, idt "foo", tkt CurlyOpen
          , idt "bar", tkt Colon, idt "baz", tkt Comma
          , tkt CurlyClose]
        @?= Right [StructDef "foo" [StructElem "bar" (TypeName "baz")]]
      doc [ tkt StructKw, idt "foo", tkt CurlyOpen
          , idt "bar", tkt Colon, idt "baz", tkt Comma
          , idt "snd", tkt Colon, idt "bool"
          , tkt CurlyClose]
        @?= Right [StructDef "foo" [StructElem "bar" (TypeName "baz"), StructElem "snd" (TypeName "bool")]]
  , "extern function" ~: do
      doc [tkt ExternKw, tkt FnKw, idt "nbcpu", tkt Colon, idt "int"] @?= Right [ExternDef $ FuncPattern [SymbolPattern "nbcpu"] (Just $ TypeName "int")]
      doc [tkt ExternKw, tkt FnKw, tkt ParenOpen, idt "a", tkt Colon, idt "int", tkt ParenClose, idt "!", tkt Colon, idt "int"] @?= Right [ExternDef $ FuncPattern [ArgPattern "a" $ Just $ TypeName "int", SymbolPattern "!"] (Just $ TypeName "int")]
  , "simple var function" ~: do
      doc [tkt FnKw, idt "key", tkt Colon, idt "int", tkt Equal, int 42]
        @?= Right [FuncDef (FuncPattern [SymbolPattern "key"] (Just $ TypeName "int")) (IntegerLit 42)]
      doc [tkt FnKw, idt "key", tkt Colon, idt "string", tkt Equal, tk "foo" StringLiter]
        @?= Right [FuncDef (FuncPattern [SymbolPattern "key"] (Just $ TypeName "string")) (StringLit "foo")]
      doc [tkt FnKw, idt "key", tkt Equal, tkt ParenOpen, int 42, tkt ParenClose]
        @?= Right [FuncDef (FuncPattern [SymbolPattern "key"] Nothing) (IntegerLit 42)]
  , "function alias" ~: do
      doc [ tkt FnKw, idt "key", tkt Equal, int 42
          , tkt FnKw, idt "alias", tkt Equal, idt "key"
          ]
        @?= Right [ FuncDef (FuncPattern [SymbolPattern "key"] Nothing) (IntegerLit 42)
                  , FuncDef (FuncPattern [SymbolPattern "alias"] Nothing) (Call "key" [])
                  ]
  , "simple prefix function" ~: do
      doc [ tkt ExternKw, tkt FnKw, idt "not", tkt UnderScore
          , tkt FnKw, idt "funny", tkt Equal, idt "not", idt "false"
          , tkt ExternKw, tkt FnKw, idt "false"
          ]
        @?= Right [ ExternDef $ FuncPattern [SymbolPattern "not", PlaceholderPattern] Nothing
                  , FuncDef (FuncPattern [SymbolPattern "funny"] Nothing) (Call "not _" [ExprCall $ Call "false" []])
                  , ExternDef $ FuncPattern [SymbolPattern "false"] Nothing
                  ]
  , "simple prefix function, with parens" ~: do
      doc [ tkt ExternKw, tkt FnKw, idt "not", tkt UnderScore
          , tkt FnKw, idt "funny", tkt Equal, idt "not", tkt ParenOpen, idt "false", tkt ParenClose
          , tkt ExternKw, tkt FnKw, idt "false"
          ]
        @?= Right [ ExternDef $ FuncPattern [SymbolPattern "not", PlaceholderPattern] Nothing
                  , FuncDef (FuncPattern [SymbolPattern "funny"] Nothing) (Call "not _" [ExprCall $ Call "false" []])
                  , ExternDef $ FuncPattern [SymbolPattern "false"] Nothing
                  ]
  , "function with ternary" ~: do
      doc [ tkt FnKw, idt "prompt", tkt Equal, idt "if", idt "tty", idt "then", tk "> " StringLiter, idt "else", tk "" StringLiter
          , tkt ExternKw, tkt FnKw, idt "tty", tkt Colon, idt "bool"
          , tkt ExternKw, tkt FnKw, idt "if", tkt UnderScore, idt "then", tkt UnderScore, idt "else", tkt UnderScore
          ]
        @?= Right [ FuncDef (FuncPattern [SymbolPattern "prompt"] Nothing) (Call "if _ then _ else _" [ExprCall $ Call "tty" [], ExprCall $ StringLit "> ", ExprCall $ StringLit ""])
                  , ExternDef $ FuncPattern [SymbolPattern "tty"] (Just $ TypeName "bool")
                  , ExternDef $ FuncPattern [SymbolPattern "if", PlaceholderPattern, SymbolPattern "then", PlaceholderPattern, SymbolPattern "else", PlaceholderPattern] Nothing
                  ]
  ]
