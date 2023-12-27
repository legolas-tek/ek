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
import qualified EKAstShowing
import qualified EKTyping

tests :: Test
tests = test
  [ "Parsing tests" ~: Parsing.tests
  , "Lisp parsing" ~: LispParsing.tests
  , "Evaluation" ~: Evaluating.tests
  , "EK Ast Show" ~: EKAstShowing.tests
  , "EK Typing" ~: EKTyping.tests
  ]

main :: IO ()
main = runTestTTAndExit tests

