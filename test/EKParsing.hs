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
import SourcePos
import EK.Ast
import Diagnostic

tk :: String -> TokenType -> Token
tk s = Token s (SourcePos "" 1 1)

tkt :: TokenType -> Token
tkt = tk ""

idt :: String -> Token
idt s = tk s TextIdentifier

int :: Int -> Token
int i = tk (show i) IntLiter

doc :: [Token] -> Either Diagnostic [Stmt Expr]
doc = parseDocument

pat :: [FuncPatternItem] -> FuncPattern
pat a = FuncPattern a Nothing Nothing

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
      doc [tkt TypeKw, idt "foo", tkt Equal, idt "aaa", tkt Arrow, idt "bbb"]
        @?= Right [TypeDef "foo" (FunctionType (TypeName "aaa") (TypeName "bbb"))]
      doc [tkt TypeKw, idt "foo", tkt Equal, idt "aaa", tkt Pipe, idt "bbb", tkt Arrow, idt "ccc", tkt Pipe, idt "ddd"]
        @?= Right [TypeDef "foo" (FunctionType (UnionType (TypeName "aaa") (TypeName "bbb")) (UnionType (TypeName "ccc") (TypeName "ddd")))]
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
      doc [tkt ExternKw, tkt FnKw, idt "nbcpu", tkt Colon, idt "int"] @?= Right [ExternDef $ FuncPattern [SymbolPattern "nbcpu"] (Just $ TypeName "int") Nothing]
      doc [tkt ExternKw, tkt FnKw, tkt ParenOpen, idt "a", tkt Colon, idt "int", tkt ParenClose, idt "!", tkt Colon, idt "int"] @?= Right [ExternDef $ FuncPattern [ArgPattern False "a" $ Just $ TypeName "int", SymbolPattern "!"] (Just $ TypeName "int") Nothing]
  , "simple var function" ~: do
      doc [tkt FnKw, idt "key", tkt Colon, idt "int", tkt Equal, int 42]
        @?= Right [FuncDef (FuncPattern [SymbolPattern "key"] (Just $ TypeName "int") Nothing) (IntegerLit 42)]
      doc [tkt FnKw, idt "key", tkt Colon, idt "string", tkt Equal, tk "foo" StringLiter]
        @?= Right [FuncDef (FuncPattern [SymbolPattern "key"] (Just $ TypeName "string") Nothing) (StringLit "foo")]
      doc [tkt FnKw, idt "key", tkt Equal, tkt ParenOpen, int 42, tkt ParenClose]
        @?= Right [FuncDef (pat [SymbolPattern "key"]) (IntegerLit 42)]
  , "Empty StructLit" ~: do
      doc [tkt FnKw, idt "foo", tkt Equal, idt "bar", tkt CurlyOpen, tkt CurlyClose]
        @?= Right [FuncDef (pat [SymbolPattern "foo"]) (StructLit "bar" [])]
  , "one int in StructLit" ~: do
      doc [tkt FnKw, idt "foo", tkt Equal, idt "bar", tkt CurlyOpen, int 42, tkt CurlyClose]
        @?= Right [FuncDef (pat [SymbolPattern "foo"]) (StructLit "bar" [IntegerLit 42])]
  , "rwo int in StructLit" ~: do
      doc [tkt FnKw, idt "foo", tkt Equal, idt "bar", tkt CurlyOpen, int 42, tkt Comma, int 43, tkt CurlyClose]
        @?= Right [FuncDef (pat [SymbolPattern "foo"]) (StructLit "bar" [IntegerLit 42, IntegerLit 43])]
  , "one string in StructLit" ~: do
      doc [tkt FnKw, idt "foo", tkt Equal, idt "bar", tkt CurlyOpen, tk "foo" StringLiter, tkt CurlyClose]
        @?= Right [FuncDef (pat [SymbolPattern "foo"]) (StructLit "bar" [StringLit "foo"])]
  , "many elements in StructLit" ~: do
      doc [tkt FnKw, idt "foo", tkt Equal, idt "bar", tkt CurlyOpen, int 42, tkt Comma, tk "foo" StringLiter, tkt Comma, int 43, tkt CurlyClose]
        @?= Right [FuncDef (pat [SymbolPattern "foo"]) (StructLit "bar" [IntegerLit 42, StringLit "foo", IntegerLit 43])]
  , "one int and one string in StructLit" ~: do
      doc [tkt FnKw, idt "foo", tkt Equal, idt "bar", tkt CurlyOpen, int 42, tkt Comma, tk "foo" StringLiter, tkt CurlyClose]
        @?= Right [FuncDef (pat [SymbolPattern "foo"]) (StructLit "bar" [IntegerLit 42, StringLit "foo"])]
  , "StructLit trailing comma" ~: do
      doc [tkt FnKw, idt "foo", tkt Equal, idt "bar", tkt CurlyOpen, int 42, tkt Comma, tk "foo" StringLiter, tkt Comma, tkt CurlyClose]
        @?= Right [FuncDef (pat [SymbolPattern "foo"]) (StructLit "bar" [IntegerLit 42, StringLit "foo"])]
  , "function alias" ~: do
      doc [ tkt FnKw, idt "key", tkt Equal, int 42
          , tkt FnKw, idt "alias", tkt Equal, idt "key"
          ]
        @?= Right [ FuncDef (pat [SymbolPattern "key"]) (IntegerLit 42)
                  , FuncDef (pat [SymbolPattern "alias"]) (Call "key" [])
                  ]
  , "simple prefix function" ~: do
      doc [ tkt ExternKw, tkt FnKw, idt "not", tkt UnderScore
          , tkt FnKw, idt "funny", tkt Equal, idt "not", idt "false"
          , tkt ExternKw, tkt FnKw, idt "false"
          ]
        @?= Right [ ExternDef $ pat [SymbolPattern "not", PlaceholderPattern]
                  , FuncDef (pat [SymbolPattern "funny"]) (Call "not _" [Call "false" []])
                  , ExternDef $ pat [SymbolPattern "false"]
                  ]
  , "simple prefix function, with parens" ~: do
      doc [ tkt ExternKw, tkt FnKw, idt "not", tkt UnderScore
          , tkt FnKw, idt "funny", tkt Equal, idt "not", tkt ParenOpen, idt "false", tkt ParenClose
          , tkt ExternKw, tkt FnKw, idt "false"
          ]
        @?= Right [ ExternDef $ pat [SymbolPattern "not", PlaceholderPattern]
                  , FuncDef (pat [SymbolPattern "funny"]) (Call "not _" [Call "false" []])
                  , ExternDef $ pat [SymbolPattern "false"]
                  ]
  , "function with ternary" ~: do
      doc [ tkt FnKw, idt "prompt", tkt Equal, idt "if", idt "tty", idt "then", tk "> " StringLiter, idt "else", tk "" StringLiter
          , tkt ExternKw, tkt FnKw, idt "tty", tkt Colon, idt "bool"
          , tkt ExternKw, tkt FnKw, idt "if", tkt UnderScore, idt "then", tkt UnderScore, idt "else", tkt UnderScore
          ]
        @?= Right [ FuncDef (pat [SymbolPattern "prompt"]) (Call "if _ then _ else _" [Call "tty" [], StringLit "> ", StringLit ""])
                  , ExternDef $ FuncPattern [SymbolPattern "tty"] (Just $ TypeName "bool") Nothing
                  , ExternDef $ pat [SymbolPattern "if", PlaceholderPattern, SymbolPattern "then", PlaceholderPattern, SymbolPattern "else", PlaceholderPattern]
                  ]
  , "simple infix function" ~: do
      doc [ tkt FnKw, tkt ParenOpen, idt "a", tkt ParenClose, idt "zero", tkt Equal, int 0
          , tkt FnKw, idt "test", tkt Equal, int 42, idt "zero"
          ]
        @?= Right [ FuncDef (pat [ArgPattern False "a" Nothing, SymbolPattern "zero"]) (IntegerLit 0)
                  , FuncDef (pat [SymbolPattern "test"]) (Call "_ zero" [IntegerLit 42])
                  ]
  , "simple infix operator" ~: do
      doc [ tkt ExternKw, tkt FnKw, tkt UnderScore, tk "+" OperatorIdentifier, tkt UnderScore
          , tkt FnKw, idt "test", tkt Equal, int 3, tk "+" OperatorIdentifier, int 7
          ]
        @?= Right [ ExternDef $ pat [PlaceholderPattern, SymbolPattern "+", PlaceholderPattern]
                  , FuncDef (pat [SymbolPattern "test"]) (Call "_ + _" [IntegerLit 3, IntegerLit 7])
                  ]
  , "double infix function" ~: do
      doc [ tkt FnKw, tkt ParenOpen, idt "a", tkt ParenClose, idt "zero", tkt Equal, int 0
          , tkt FnKw, idt "test", tkt Equal, int 42, idt "zero", idt "zero"
          ]
        @?= Right [ FuncDef (pat [ArgPattern False "a" Nothing, SymbolPattern "zero"]) (IntegerLit 0)
                  , FuncDef (pat [SymbolPattern "test"]) (Call "_ zero" [Call "_ zero" [IntegerLit 42]])
                  ]
  , "simple infix function using arg" ~: do
      doc [ tkt FnKw, tkt ParenOpen, idt "a", tkt ParenClose, idt "qed", tkt Equal, idt "a"
          , tkt FnKw, idt "test", tkt Equal, int 42, idt "qed"
          ]
        @?= Right [ FuncDef (pat [ArgPattern False "a" Nothing, SymbolPattern "qed"]) (Call "a" [])
                  , FuncDef (pat [SymbolPattern "test"]) (Call "_ qed" [IntegerLit 42])
                  ]
  , "double prefix function" ~: do
      doc [ tkt FnKw, idt "zero", tkt ParenOpen, idt "a", tkt ParenClose, tkt Equal, int 0
          , tkt FnKw, idt "test", tkt Equal, idt "zero", idt "zero", int 42
          ]
        @?= Right [ FuncDef (pat [SymbolPattern "zero", ArgPattern False "a" Nothing]) (IntegerLit 0)
                  , FuncDef (pat [SymbolPattern "test"]) (Call "zero _" [Call "zero _" [IntegerLit 42]])
                  ]
  , "prefix function with 2 unseparated args" ~: do
      doc [ tkt FnKw, idt "add", tkt UnderScore, tkt UnderScore, tkt Equal, int 3
          , tkt FnKw, idt "test", tkt Equal, idt "add", int 1, int 2
          ]
        @?= Right [ FuncDef (pat [SymbolPattern "add", PlaceholderPattern, PlaceholderPattern]) (IntegerLit 3)
                  , FuncDef (pat [SymbolPattern "test"]) (Call "add _ _" [IntegerLit 1, IntegerLit 2])
                  ]
  , "postfix function with 2 unseparated args" ~: do
      doc [ tkt FnKw, tkt UnderScore, tkt UnderScore, idt "add", tkt Equal, int 3
          , tkt FnKw, idt "test", tkt Equal, int 1, int 2, idt "add"
          ]
        @?= Right [ FuncDef (pat [PlaceholderPattern, PlaceholderPattern, SymbolPattern "add"]) (IntegerLit 3)
                  , FuncDef (pat [SymbolPattern "test"]) (Call "_ _ add" [IntegerLit 1, IntegerLit 2])
                  ]
  , "postfix function with 2 unseparated args, multiple" ~: do
      doc [ tkt FnKw, tkt UnderScore, tkt UnderScore, idt "add", tkt Equal, int 3
          , tkt FnKw, tkt UnderScore, tkt UnderScore, idt "sub", tkt Equal, int 2
          , tkt FnKw, idt "test", tkt Equal, int 1, int 2, idt "sub", int 3, idt "add"
          ]
        @?= Right [ FuncDef (pat [PlaceholderPattern, PlaceholderPattern, SymbolPattern "add"]) (IntegerLit 3)
                  , FuncDef (pat [PlaceholderPattern, PlaceholderPattern, SymbolPattern "sub"]) (IntegerLit 2)
                  , FuncDef (pat [SymbolPattern "test"]) (Call "_ _ add" [Call "_ _ sub" [IntegerLit 1, IntegerLit 2], IntegerLit 3])
                  ]
  , "prefix function with 2 unseparated args, multiple" ~: do
      doc [ tkt FnKw, idt "add", tkt UnderScore, tkt UnderScore, tkt Equal, int 3
          , tkt FnKw, idt "sub", tkt UnderScore, tkt UnderScore, tkt Equal, int 2
          , tkt FnKw, idt "test", tkt Equal, idt "add", idt "sub", int 1, int 2, int 3
          ]
        @?= Right [ FuncDef (pat [SymbolPattern "add", PlaceholderPattern, PlaceholderPattern]) (IntegerLit 3)
                  , FuncDef (pat [SymbolPattern "sub", PlaceholderPattern, PlaceholderPattern]) (IntegerLit 2)
                  , FuncDef (pat [SymbolPattern "test"]) (Call "add _ _" [Call "sub _ _" [IntegerLit 1, IntegerLit 2], IntegerLit 3])
                  ]
  , "ArgPattern parsing lazy" ~: do
      doc [ tkt FnKw, tkt ParenOpen, tkt LazyKw, idt "a", tkt Colon, idt "int", tkt ParenClose, idt "zero", tkt Equal, int 0
          ]
        @?= Right [ FuncDef (pat [ArgPattern True "a" (Just $ TypeName "int"), SymbolPattern "zero"]) (IntegerLit 0)
                  ]
  , "ArgPattern parsing not lazy" ~: do
      doc [ tkt FnKw, tkt ParenOpen, idt "a", tkt Colon, idt "int", tkt ParenClose, idt "zero", tkt Equal, int 0
          ]
        @?= Right [ FuncDef (pat [ArgPattern False "a" (Just $ TypeName "int"), SymbolPattern "zero"]) (IntegerLit 0)
                  ]
  , "atom reference" ~: do
      doc [ tkt FnKw, idt "fruit", tkt Equal, idt "apple"
          , tkt AtomKw, idt "apple"
          ]
        @?= Right [ FuncDef (pat [SymbolPattern "fruit"]) (Call "apple" [])
                  , AtomDef "apple"
                  ]
  , "error case, invalid placeholder" ~: do
      doc [ tkt FnKw, idt "add", tkt Equal, tkt UnderScore
          ]
        @?= Left (Diagnostic Error "Invalid placeholder" (SourcePos "" 1 1))
  , "error case, variable does not exist" ~: do
      doc [ tkt FnKw, idt "add", tkt Equal, idt "a"
          ]
        @?= Left (Diagnostic Error "Could not resolve expression" (SourcePos "" 1 1))
  , "error case, unopened paren" ~: do
      doc [ tkt FnKw, idt "a", tkt Equal, int 3, tkt CurlyClose
          ]
        @?= Left (Diagnostic Error "Unexpected trailing token" (SourcePos "" 1 1))
  , "precedence, + and *" ~: do
      doc [ tkt ExternKw, tkt FnKw, tkt UnderScore, idt "*", tkt UnderScore, tkt PrecedenceKw, int 7
          , tkt ExternKw, tkt FnKw, tkt UnderScore, idt "+", tkt UnderScore, tkt PrecedenceKw, int 6
          , tkt FnKw, idt "test", tkt Equal, int 1, idt "+", int 2, idt "*", int 3, idt "+", int 4
          ]
        @?= Right [ ExternDef (FuncPattern [PlaceholderPattern, SymbolPattern "*", PlaceholderPattern] Nothing (Just 7))
                  , ExternDef (FuncPattern [PlaceholderPattern, SymbolPattern "+", PlaceholderPattern] Nothing (Just 6))
                  , FuncDef (pat [SymbolPattern "test"])
                    (Call ("_ + _" `precedence` 6)
                     [ Call ("_ + _" `precedence` 6)
                       [ IntegerLit 1
                       , Call ("_ * _" `precedence` 7)
                         [ IntegerLit 2, IntegerLit 3]
                       ]
                     , IntegerLit 4
                     ]
                    )
                  ]
  , "precedence, + and * with parens" ~: do
      doc [ tkt ExternKw, tkt FnKw, tkt UnderScore, idt "*", tkt UnderScore, tkt PrecedenceKw, int 7
          , tkt ExternKw, tkt FnKw, tkt UnderScore, idt "+", tkt UnderScore, tkt PrecedenceKw, int 6
          , tkt FnKw, idt "test", tkt Equal, int 1, idt "+", int 2, idt "*", tkt ParenOpen, int 3, idt "+", int 4, tkt ParenClose
          ]
        @?= Right [ ExternDef (FuncPattern [PlaceholderPattern, SymbolPattern "*", PlaceholderPattern] Nothing (Just 7))
                  , ExternDef (FuncPattern [PlaceholderPattern, SymbolPattern "+", PlaceholderPattern] Nothing (Just 6))
                  , FuncDef (pat [SymbolPattern "test"])
                    (Call ("_ + _" `precedence` 6)
                     [ IntegerLit 1
                     , Call ("_ * _" `precedence` 7)
                       [ IntegerLit 2
                       , Call ("_ + _" `precedence` 6)
                         [ IntegerLit 3
                         , IntegerLit 4
                         ]
                       ]
                     ]
                    )
                  ]
  , "precedence, boolean not and and" ~: do
      doc [ tkt ExternKw, tkt FnKw, tkt UnderScore, idt "and", tkt UnderScore, tkt PrecedenceKw, int 3
          , tkt ExternKw, tkt FnKw, idt "not", tkt UnderScore
          , tkt FnKw, idt "test", tkt Equal, idt "not", int 1, idt "and", int 2
          ]
        @?= Right [ ExternDef (FuncPattern [PlaceholderPattern, SymbolPattern "and", PlaceholderPattern] Nothing (Just 3))
                  , ExternDef (pat [SymbolPattern "not", PlaceholderPattern])
                  , FuncDef (pat [SymbolPattern "test"])
                    (Call ("_ and _" `precedence` 3)
                     [ Call "not _" [IntegerLit 1]
                     , IntegerLit 2
                     ]
                    )
                  ]
  , "precedence, boolean not and and, loose not" ~: do
      doc [ tkt ExternKw, tkt FnKw, tkt UnderScore, idt "and", tkt UnderScore, tkt PrecedenceKw, int 3
          , tkt ExternKw, tkt FnKw, idt "not", tkt UnderScore, tkt PrecedenceKw, int 2
          , tkt FnKw, idt "test", tkt Equal, idt "not", int 1, idt "and", int 2
          ]
        @?= Right [ ExternDef (FuncPattern [PlaceholderPattern, SymbolPattern "and", PlaceholderPattern] Nothing (Just 3))
                  , ExternDef (FuncPattern [SymbolPattern "not", PlaceholderPattern] Nothing (Just 2))
                  , FuncDef (pat [SymbolPattern "test"])
                    (Call ("not _" `precedence` 2)
                     [ Call ("_ and _" `precedence` 3)
                       [IntegerLit 1, IntegerLit 2]
                     ]
                    )
                  ]
  , "precedence with if and eq" ~: do
      doc [ tkt ExternKw, tkt FnKw, tkt UnderScore, idt "eq", tkt UnderScore, tkt PrecedenceKw, int 4
          , tkt ExternKw, tkt FnKw, idt "if", tkt UnderScore, idt "then", tkt UnderScore, idt "else", tkt UnderScore, tkt PrecedenceKw, int 1
          , tkt FnKw, idt "test", tkt Equal, idt "if", int 1, idt "eq", int 2, idt "then", int 3, idt "else", int 4
          ]
        @?= Right [ ExternDef (FuncPattern [PlaceholderPattern, SymbolPattern "eq", PlaceholderPattern] Nothing (Just 4))
                  , ExternDef (FuncPattern [SymbolPattern "if", PlaceholderPattern, SymbolPattern "then", PlaceholderPattern, SymbolPattern "else", PlaceholderPattern] Nothing (Just 1))
                  , FuncDef (pat [SymbolPattern "test"])
                    (Call ("if _ then _ else _" `precedence` 1)
                     [ Call ("_ eq _" `precedence` 4)
                       [IntegerLit 1, IntegerLit 2]
                     , IntegerLit 3
                     , IntegerLit 4
                     ]
                    )
                  ]
  , "placeholder in call" ~: do
      doc [ tkt FnKw, idt "test", tkt Equal, idt "a", tkt UnderScore
          , tkt FnKw, idt "a", tkt UnderScore, tkt Equal, int 1
          ]
        @?= Right [ FuncDef (pat [SymbolPattern "test"])
                    (Lambda "$0" (Call "a _" [Call "$0" []]))
                  , FuncDef (pat [SymbolPattern "a", PlaceholderPattern])
                    (IntegerLit 1)
                  ]
  , "2 placeholders in call" ~: do
      doc [ tkt ExternKw, tkt FnKw, tkt UnderScore, idt "plus", tkt UnderScore
          , tkt FnKw, idt "addition", tkt Equal, tkt UnderScore, idt "plus", tkt UnderScore
          ]
        @?= Right [ ExternDef (pat [PlaceholderPattern, SymbolPattern "plus", PlaceholderPattern])
                  , FuncDef (pat [SymbolPattern "addition"])
                    (Lambda "$0" (Lambda "$1" (Call "_ plus _" [Call "$0" [], Call "$1" []])))
                  ]
  , "simple lambda" ~: do
      doc [ tkt FnKw, idt "id", tkt Equal, tkt Backslash, idt "x", tkt Equal, idt "x"
          ]
        @?= Right [ FuncDef (pat [SymbolPattern "id"])
                    (Lambda "x" (Call "x" []))
                  ]
  , "2 args lambda" ~: do
      doc [ tkt FnKw, idt "const", tkt Equal, tkt Backslash, idt "x", idt "y", tkt Equal, idt "x"
          ]
        @?= Right [ FuncDef (pat [SymbolPattern "const"])
                    (Lambda "x" (Lambda "y" (Call "x" [])))
                  ]
  , "nested lambda" ~: do
      doc [ tkt FnKw, idt "const", tkt Equal, tkt Backslash, idt "x", tkt Equal, tkt Backslash, idt "y", tkt Equal, idt "x"
          ]
        @?= Right [ FuncDef (pat [SymbolPattern "const"])
                    (Lambda "x" (Lambda "y" (Call "x" [])))
                  ]
  , "addition lambda" ~: do
      doc [ tkt ExternKw, tkt FnKw, tkt UnderScore, idt "+", tkt UnderScore
          , tkt FnKw, idt "addition", tkt Equal, tkt Backslash, idt "x", idt "y", tkt Equal, idt "x", idt "+", idt "y"
          ]
        @?= Right [ ExternDef (pat [PlaceholderPattern, SymbolPattern "+", PlaceholderPattern])
                  , FuncDef (pat [SymbolPattern "addition"])
                    (Lambda "x" (Lambda "y" (Call "_ + _" [Call "x" [], Call "y" []])))
                  ]
  , "typedef + fn" ~: do
      doc [ tkt TypeKw, idt "integer", tkt Equal, idt "int"
          , tkt FnKw, tkt UnderScore, idt "one", tkt Equal, int 1
          ]
        @?= Right [ TypeDef "integer" (TypeName "int")
                  , FuncDef (pat [PlaceholderPattern, SymbolPattern "one"])
                    (IntegerLit 1)
                  ]
  ]
