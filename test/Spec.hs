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
  ]

main :: IO Counts
main = runTestTT tests

