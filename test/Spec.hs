{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

import Test.HUnit

import qualified Parsing
import qualified LispParsing

tests :: Test
tests = test
  [ "Parsing tests" ~: Parsing.tests
  , "Lisp parsing" ~: LispParsing.tests
  ]

main :: IO ()
main = runTestTTAndExit tests

