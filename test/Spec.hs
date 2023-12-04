{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

import Test.HUnit

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
  ]

main :: IO Counts
main = runTestTT tests

