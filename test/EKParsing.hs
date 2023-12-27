{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module EKParsing (tests) where

import Test.HUnit

import EK.Parser
import Token
import EK.Ast
import Parser

tk :: String -> TokenType -> Token
tk s = Token s 0 0 ""

tkt :: TokenType -> Token
tkt = tk ""

idt :: String -> Token
idt s = tk s TextIdentifier

-- this is bad, but it's just for testing
instance MonadFail (Either String) where
  fail = Left

doc :: [Token] -> Either String [Stmt]
doc t = do
  (a, []) <- runParser parseDocument t
  return a

tests :: Test
tests = test
  [ "atom" ~: do
      doc [tkt AtomKw, idt "foo"] @?= Right [AtomDef "foo"]
  , "type" ~: do
      doc [tkt TypeKw, idt "foo", tkt Equal, idt "bar"] @?= Right [TypeDef "foo" (TypeName "bar")]
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
  ]
