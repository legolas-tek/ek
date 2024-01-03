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

tests :: Test
tests = test
  [ "parseChar" ~: do
      let p = parseChar 'a'
      runParser p "abc"  @?= Right ('a', "bc")
      runParser p "aaaa" @?= Right ('a', "aaa")
      runParser p "def"  @?= Left "Expected 'a' but found 'd' at: :1:1"
      runParser p ""     @?= Left "Expected 'a' but found EOF at: :1:1"
  , "parseAnyChar" ~: do
      runParser (parseAnyChar "bca") "abcd" @?= Right ('a', "bcd")
      runParser (parseAnyChar "xyz") "abcd" @?= Left "Expected one of 'xyz' but found 'a' at: :1:1"
      runParser (parseAnyChar "bca") "cdef" @?= Right ('c', "def")
  , "parseOr" ~: do
      let p = parseChar 'a' <|> parseChar 'b'
      runParser p "abcd" @?= Right ('a', "bcd")
      runParser p "bcda" @?= Right ('b', "cda")
      runParser p "xyz"  @?= Left "Expected 'b' but found 'x' at: :1:1"
  , "parseAnd" ~: do
      let p = parseChar 'a' >>= \a -> parseChar 'b' >>= \b -> return (a, b)
      runParser p "abcd" @?= Right (('a', 'b'), "cd")
      runParser p "bcda" @?= Left "Expected 'a' but found 'b' at: :1:1"
      runParser p "acd"  @?= Left "Expected 'b' but found 'c' at: :1:2"
      runParser p "xyz"  @?= Left "Expected 'a' but found 'x' at: :1:1"
  , "spaces" ~: do
      runParser spaces "      foobar" @?= Right ("      ", "foobar")
      runParser spaces "foobar      " @?= Right ("", "foobar      ")
  , "parseSome" ~: do
      let digits = some (parseAnyChar ['0'..'9'])
      runParser digits "42foobar" @?= Right ("42", "foobar")
      runParser digits "foobar42" @?= Left "Expected one of '0123456789' but found 'f' at: :1:1"
  , "parseUInt" ~: do
      runParser parseUInt "42foobar" @?= Right (42, "foobar")
      runParser parseUInt "23894732foobar" @?= Right (23894732, "foobar")
      runParser parseUInt "0a" @?= Right (0, "a")
      runParser parseUInt "foobar42" @?= Left "Expected one of '0123456789' but found 'f' at: :1:1"
  , "parseInt" ~: do
      runParser parseInt "42foobar" @?= Right (42, "foobar")
      runParser parseInt "-42foobar" @?= Right (-42, "foobar")
      runParser parseInt "23894732foobar" @?= Right (23894732, "foobar")
      runParser parseInt "-23894732foobar" @?= Right (-23894732, "foobar")
      runParser parseInt "0a" @?= Right (0, "a")
      runParser parseInt "-0a" @?= Right (0, "a")
      isLeft (runParser parseInt "foobar42") @?= True
      isLeft (runParser parseInt "--42foobar") @?= True
  , "parseList" ~: do
      runParser (parseList parseInt) "(123 456)foo bar" @?= Right ([123, 456], "foo bar")
      runParser (parseList parseInt) "(1 2 3 5 7 11 13 17)" @?= Right ([1, 2, 3, 5, 7, 11, 13, 17], "")
  ]
