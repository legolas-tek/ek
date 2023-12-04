{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

import Test.HUnit
import Data.Either (isLeft)

import Parser

tests :: Test
tests = test
  [ "parseChar" ~: do
      parseChar 'a' "abc"  @?= Right ('a', "bc")
      parseChar 'a' "aaaa" @?= Right ('a', "aaa")
      parseChar 'a' "def"  @?= Left "Expected 'a' but found 'd'"
      parseChar 'a' ""     @?= Left "Expected 'a' but found EOF"
  , "parseAnyChar" ~: do
      parseAnyChar "bca" "abcd" @?= Right ('a', "bcd")
      parseAnyChar "xyz" "abcd" @?= Left "Expected one of 'xyz' but found 'a'"
      parseAnyChar "bca" "cdef" @?= Right ('c', "def")
  , "parseOr" ~: do
      parseOr (parseChar 'a') (parseChar 'b') "abcd" @?= Right ('a', "bcd")
      parseOr (parseChar 'a') (parseChar 'b') "bcda" @?= Right ('b', "cda")
      parseOr (parseChar 'a') (parseChar 'b') "xyz"  @?= Left "Expected 'a' but found 'x' or Expected 'b' but found 'x'"
  , "parseAnd" ~: do
      parseAnd (parseChar 'a') (parseChar 'b') "abcd" @?= Right (('a', 'b'), "cd")
      parseAnd (parseChar 'a') (parseChar 'b') "bcda" @?= Left "Expected 'a' but found 'b'"
      parseAnd (parseChar 'a') (parseChar 'b') "acd"  @?= Left "Expected 'b' but found 'c'"
      parseAnd (parseChar 'a') (parseChar 'b') "xyz"  @?= Left "Expected 'a' but found 'x'"
  , "parseAndWith" ~: do
      parseAndWith (\ x y -> [x, y]) (parseChar 'a') (parseChar 'b') "abcd" @?= Right ("ab", "cd")
  , "parseMany" ~: do
      parseMany (parseChar ' ') "      foobar" @?= Right ("      ", "foobar")
      parseMany (parseChar ' ') "foobar      " @?= Right ("", "foobar      ")
  , "parseSome" ~: do
      parseSome (parseAnyChar ['0'..'9']) "42foobar" @?= Right ("42", "foobar")
      parseSome (parseAnyChar ['0'..'9']) "foobar42" @?= Left "Expected one of '0123456789' but found 'f'"
  , "parseUInt" ~: do
      parseUInt "42foobar" @?= Right (42, "foobar")
      parseUInt "23894732foobar" @?= Right (23894732, "foobar")
      parseUInt "0a" @?= Right (0, "a")
      parseUInt "foobar42" @?= Left "Expected one of '0123456789' but found 'f'"
  , "parseInt" ~: do
      parseInt "42foobar" @?= Right (42, "foobar")
      parseInt "-42foobar" @?= Right (-42, "foobar")
      parseInt "23894732foobar" @?= Right (23894732, "foobar")
      parseInt "-23894732foobar" @?= Right (-23894732, "foobar")
      parseInt "0a" @?= Right (0, "a")
      parseInt "-0a" @?= Right (0, "a")
      (isLeft $ parseInt "foobar42") @?= True
      (isLeft $ parseInt "--42foobar") @?= True
  ]

main :: IO Counts
main = runTestTT tests

