{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

import Test.HUnit

import qualified Parsing
import qualified LispParsing
import qualified Evaluating
import qualified Executing

tests :: Test
tests = test
  [ "Parsing tests" ~: Parsing.tests
  , "Lisp parsing" ~: LispParsing.tests
  , "Evaluation" ~: Evaluating.tests
  , "Executing" ~: Executing.tests
  ]

main :: IO ()
main = runTestTTAndExit tests

