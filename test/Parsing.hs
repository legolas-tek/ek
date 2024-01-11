{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

module Parsing (tests) where

import Test.HUnit
import Data.Either (isLeft)

import Parser

import Diagnostic
import SourcePos

tests :: Test
tests = test
  [ "parseChar" ~: do
      let p = parseChar 'a'
      runParser p "abc"  @?= Right ('a', "bc")
      runParser p "aaaa" @?= Right ('a', "aaa")
      runParser p "def"  @?= Left (Diagnostic Error "Expected 'a' but found 'd'" (SourcePos "" 1 1))
      runParser p ""     @?= Left (Diagnostic Error "Expected 'a' but found EOF" (SourcePos "" 1 1))
  , "parseAnyChar" ~: do
      runParser (parseAnyChar "bca") "abcd" @?= Right ('a', "bcd")
      runParser (parseAnyChar "xyz") "abcd" @?= Left (Diagnostic Error "Expected one of 'xyz' but found 'a'" (SourcePos "" 1 1))
      runParser (parseAnyChar "bca") "cdef" @?= Right ('c', "def")
  , "parseOr" ~: do
      let p = parseChar 'a' <|> parseChar 'b'
      runParser p "abcd" @?= Right ('a', "bcd")
      runParser p "bcda" @?= Right ('b', "cda")
      runParser p "xyz"  @?= Left (Diagnostic Error "Expected 'b' but found 'x'" (SourcePos "" 1 1))
  , "parseAnd" ~: do
      let p = parseChar 'a' >>= \a -> parseChar 'b' >>= \b -> return (a, b)
      runParser p "abcd" @?= Right (('a', 'b'), "cd")
      runParser p "bcda" @?= Left (Diagnostic Error "Expected 'a' but found 'b'" (SourcePos "" 1 1))
      runParser p "acd"  @?= Left (Diagnostic Error "Expected 'b' but found 'c'" (SourcePos "" 1 2))
      runParser p "xyz"  @?= Left (Diagnostic Error "Expected 'a' but found 'x'" (SourcePos "" 1 1))
  , "spaces" ~: do
      runParser spaces "      foobar" @?= Right ("      ", "foobar")
      runParser spaces "foobar      " @?= Right ("", "foobar      ")
  , "parseSome" ~: do
      let digits = some (parseAnyChar ['0'..'9'])
      runParser digits "42foobar" @?= Right ("42", "foobar")
      runParser digits "foobar42" @?= Left (Diagnostic Error "Expected one of '0123456789' but found 'f'" (SourcePos "" 1 1))
  , "parseUInt" ~: do
      runParser parseUInt "42foobar" @?= Right (42, "foobar")
      runParser parseUInt "23894732foobar" @?= Right (23894732, "foobar")
      runParser parseUInt "0a" @?= Right (0, "a")
      runParser parseUInt "foobar42" @?= Left (Diagnostic Error "Expected one of '0123456789' but found 'f'" (SourcePos "" 1 1))
  , "parseInt" ~: do
      runParser parseInt "42foobar" @?= Right (42, "foobar")
      runParser parseInt "-42foobar" @?= Right (-42, "foobar")
      runParser parseInt "23894732foobar" @?= Right (23894732, "foobar")
      runParser parseInt "-23894732foobar" @?= Right (-23894732, "foobar")
      runParser parseInt "0a" @?= Right (0, "a")
      runParser parseInt "-0a" @?= Right (0, "a")
      isLeft (runParser parseInt "foobar42") @?= True
      isLeft (runParser parseInt "--42foobar") @?= True
  , "parseUFloat" ~: do
      runParser parseUFloat "42.55foobar" @?= Right (42.55 , "foobar")
      runParser parseUFloat "42foobar" @?= Right (42, "foobar")
      isLeft (runParser parseUFloat "foobar42.5") @?= True
  , "parseFloat" ~: do
      runParser parseFloat "42.5foobar" @?= Right (42.5, "foobar")
      runParser parseFloat "-42.5foobar" @?= Right (-42.5, "foobar")
      isLeft (runParser parseInt "foobar-42.5") @?= True
      isLeft (runParser parseInt "--42.5foobar") @?= True
  , "parseList" ~: do
      runParser (parseList parseInt) "(123 456)foo bar" @?= Right ([123, 456], "foo bar")
      runParser (parseList parseInt) "(1 2 3 5 7 11 13 17)" @?= Right ([1, 2, 3, 5, 7, 11, 13, 17], "")
  ]
