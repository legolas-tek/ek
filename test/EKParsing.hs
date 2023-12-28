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
doc = parseDocument

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
  , "simple infix function" ~: do
      doc [ tkt FnKw, tkt ParenOpen, idt "a", tkt ParenClose, idt "zero", tkt Equal, int 0
          , tkt FnKw, idt "test", tkt Equal, int 42, idt "zero"
          ]
        @?= Right [ FuncDef (FuncPattern [ArgPattern "a" Nothing, SymbolPattern "zero"] Nothing) (IntegerLit 0)
                  , FuncDef (FuncPattern [SymbolPattern "test"] Nothing) (Call "_ zero" [ExprCall $ IntegerLit 42])
                  ]
  , "simple infix operator" ~: do
      doc [ tkt ExternKw, tkt FnKw, tkt UnderScore, tk "+" OperatorIdentifier, tkt UnderScore
          , tkt FnKw, idt "test", tkt Equal, int 3, tk "+" OperatorIdentifier, int 7
          ]
        @?= Right [ ExternDef $ FuncPattern [PlaceholderPattern, SymbolPattern "+", PlaceholderPattern] Nothing
                  , FuncDef (FuncPattern [SymbolPattern "test"] Nothing) (Call "_ + _" [ExprCall $ IntegerLit 3, ExprCall $ IntegerLit 7])
                  ]
  , "double infix function" ~: do
      doc [ tkt FnKw, tkt ParenOpen, idt "a", tkt ParenClose, idt "zero", tkt Equal, int 0
          , tkt FnKw, idt "test", tkt Equal, int 42, idt "zero", idt "zero"
          ]
        @?= Right [ FuncDef (FuncPattern [ArgPattern "a" Nothing, SymbolPattern "zero"] Nothing) (IntegerLit 0)
                  , FuncDef (FuncPattern [SymbolPattern "test"] Nothing) (Call "_ zero" [ExprCall $ Call "_ zero" [ExprCall $ IntegerLit 42]])
                  ]
  , "simple infix function using arg" ~: do
      doc [ tkt FnKw, tkt ParenOpen, idt "a", tkt ParenClose, idt "qed", tkt Equal, idt "a"
          , tkt FnKw, idt "test", tkt Equal, int 42, idt "qed"
          ]
        @?= Right [ FuncDef (FuncPattern [ArgPattern "a" Nothing, SymbolPattern "qed"] Nothing) (Call "a" [])
                  , FuncDef (FuncPattern [SymbolPattern "test"] Nothing) (Call "_ qed" [ExprCall $ IntegerLit 42])
                  ]
  , "double prefix function" ~: do
      doc [ tkt FnKw, idt "zero", tkt ParenOpen, idt "a", tkt ParenClose, tkt Equal, int 0
          , tkt FnKw, idt "test", tkt Equal, idt "zero", idt "zero", int 42
          ]
        @?= Right [ FuncDef (FuncPattern [SymbolPattern "zero", ArgPattern "a" Nothing] Nothing) (IntegerLit 0)
                  , FuncDef (FuncPattern [SymbolPattern "test"] Nothing) (Call "zero _" [ExprCall $ Call "zero _" [ExprCall $ IntegerLit 42]])
                  ]
  , "prefix function with 2 unseparated args" ~: do
      doc [ tkt FnKw, idt "add", tkt UnderScore, tkt UnderScore, tkt Equal, int 3
          , tkt FnKw, idt "test", tkt Equal, idt "add", int 1, int 2
          ]
        @?= Right [ FuncDef (FuncPattern [SymbolPattern "add", PlaceholderPattern, PlaceholderPattern] Nothing) (IntegerLit 3)
                  , FuncDef (FuncPattern [SymbolPattern "test"] Nothing) (Call "add _ _" [ExprCall $ IntegerLit 1, ExprCall $ IntegerLit 2])
                  ]
  , "postfix function with 2 unseparated args" ~: do
      doc [ tkt FnKw, tkt UnderScore, tkt UnderScore, idt "add", tkt Equal, int 3
          , tkt FnKw, idt "test", tkt Equal, int 1, int 2, idt "add"
          ]
        @?= Right [ FuncDef (FuncPattern [PlaceholderPattern, PlaceholderPattern, SymbolPattern "add"] Nothing) (IntegerLit 3)
                  , FuncDef (FuncPattern [SymbolPattern "test"] Nothing) (Call "_ _ add" [ExprCall $ IntegerLit 1, ExprCall $ IntegerLit 2])
                  ]
  , "postfix function with 2 unseparated args, multiple" ~: do
      doc [ tkt FnKw, tkt UnderScore, tkt UnderScore, idt "add", tkt Equal, int 3
          , tkt FnKw, tkt UnderScore, tkt UnderScore, idt "sub", tkt Equal, int 2
          , tkt FnKw, idt "test", tkt Equal, int 1, int 2, idt "sub", int 3, idt "add"
          ]
        @?= Right [ FuncDef (FuncPattern [PlaceholderPattern, PlaceholderPattern, SymbolPattern "add"] Nothing) (IntegerLit 3)
                  , FuncDef (FuncPattern [PlaceholderPattern, PlaceholderPattern, SymbolPattern "sub"] Nothing) (IntegerLit 2)
                  , FuncDef (FuncPattern [SymbolPattern "test"] Nothing) (Call "_ _ add" [ExprCall $ Call "_ _ sub" [ExprCall $ IntegerLit 1, ExprCall $ IntegerLit 2], ExprCall $ IntegerLit 3])
                  ]
  , "prefix function with 2 unseparated args, multiple" ~: do
      doc [ tkt FnKw, idt "add", tkt UnderScore, tkt UnderScore, tkt Equal, int 3
          , tkt FnKw, idt "sub", tkt UnderScore, tkt UnderScore, tkt Equal, int 2
          , tkt FnKw, idt "test", tkt Equal, idt "add", idt "sub", int 1, int 2, int 3
          ]
        @?= Right [ FuncDef (FuncPattern [SymbolPattern "add", PlaceholderPattern, PlaceholderPattern] Nothing) (IntegerLit 3)
                  , FuncDef (FuncPattern [SymbolPattern "sub", PlaceholderPattern, PlaceholderPattern] Nothing) (IntegerLit 2)
                  , FuncDef (FuncPattern [SymbolPattern "test"] Nothing) (Call "add _ _" [ExprCall $ Call "sub _ _" [ExprCall $ IntegerLit 1, ExprCall $ IntegerLit 2], ExprCall $ IntegerLit 3])
                  ]
  , "error case, invalid placeholder" ~: do
      doc [ tkt FnKw, idt "add", tkt Equal, tkt UnderScore
          ]
        @?= Left "Invalid placeholder"
  , "error case, variable does not exist" ~: do
      doc [ tkt FnKw, idt "add", tkt Equal, idt "a"
          ]
        @?= Left "Could not resolve expression"
  , "error case, unopened paren" ~: do
      doc [ tkt FnKw, idt "a", tkt Equal, int 3, tkt CurlyClose
          ]
        @?= Left "Unexpected trailing token"
  ]
