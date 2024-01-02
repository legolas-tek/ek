{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

module LispParsing (tests) where

import Test.HUnit

import Parser
import Lisp
import SExpr

tests :: Test
tests = test
  [ "integer" ~: do
      runParser parseSExpr "42"     @?= Right (IntegerLit 42, "")
      runParser parseSExpr "-84"    @?= Right (IntegerLit (-84), "")
      runParser parseSExpr "   -25" @?= Right (IntegerLit (-25), "")
      runParser parseSExpr " 3 "    @?= Right (IntegerLit 3, " ")
  , "symbol" ~: do
      runParser parseSExpr "helloWorld" @?= Right (Symbol "helloWorld", "")
      runParser parseSExpr " +" @?= Right (Symbol "+", "")
      runParser parseSExpr "+-+" @?= Right (Symbol "+-+", "")
      runParser parseSExpr "-" @?= Right (Symbol "-", "")
      runParser parseSExpr "-y-" @?= Right (Symbol "-y-", "")
  , "list" ~: do
      runParser parseSExpr "(+ 2 3)" @?= Right (List [Symbol "+", IntegerLit 2, IntegerLit 3], "")
      runParser parseSExpr "(+ (- 7 2) 3)" @?= Right (List [Symbol "+", (List [Symbol "-", IntegerLit 7, IntegerLit 2]), IntegerLit 3], "")
      runParser parseSExpr "(+(- 7 2)3)" @?= Right (List [Symbol "+", (List [Symbol "-", IntegerLit 7, IntegerLit 2]), IntegerLit 3], "")
      runParser parseSExpr " (+   (- 7 2)  3  )" @?= Right (List [Symbol "+", (List [Symbol "-", IntegerLit 7, IntegerLit 2]), IntegerLit 3], "")
  , "string" ~: do
      runParser parseSExpr "\"hello world\"" @?= Right (StringLit "hello world", "")
      runParser parseSExpr "(funct \"string1\")" @?= Right (List [Symbol "funct", StringLit "string1"], "")
      runParser parseSExpr "(funct \"string1\" \"string2\")" @?= Right (List [Symbol "funct", StringLit "string1", StringLit "string2"], "")
      runParser parseStringLit "\"string1\"" @?= Right ("string1", "")
      runParser parseStringLit "\"string1\" " @?= Right ("string1", " ")
      runParser parseStringLit "\"string\\0\"" @?= Right ("string\0", "")
      runParser parseStringLit "\"string1\\\"\"" @?= Right ("string1\"", "")
      runParser parseStringLit "\"string1\\t\"" @?= Right ("string1\t", "")
      runParser parseStringLit "\"string1\\n\"" @?= Right ("string1\n", "")
      runParser parseStringLit "\"string1\\r\"" @?= Right ("string1\r", "")
      runParser parseStringLit "\"string1\\\\\"" @?= Right ("string1\\", "")
  , "bloc comments" ~: do
      runParser (many parseSExpr) "#|test|#3" @?= Right ([IntegerLit 3], "")
      runParser parseSExpr "#|test|#abc" @?= Right (Symbol "abc", "")
      runParser parseSExpr "(define #|test|#a 1)" @?=
            Right (List [Symbol "define",Symbol "a",IntegerLit 1],"")
  , "line comments" ~: do
      runParser (many parseSExpr) ";test\na" @?= Right ([Symbol "a"], "")
      runParser (many parseSExpr) "test;test" @?= Right ([Symbol "test"], ";test")
  , "consecutive comments" ~: do
      runParser (many parseSExpr) "(define #|test1|##|test2|#  #|test3|#a 1)" @?=
            Right ([List [Symbol "define",Symbol "a",IntegerLit 1]],"")
  , "nested comments" ~: do
      runParser parseSExpr "(define #|te#|test2|#st1|#a 1)" @?=
            Right (List [Symbol "define",Symbol "a",IntegerLit 1],"")
  ]
