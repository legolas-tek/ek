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
import qualified EKAstShowing
import qualified EKParsing
import qualified EKTyping

tests :: Test
tests = test
  [ "Parsing tests" ~: Parsing.tests
  , "Lisp parsing" ~: LispParsing.tests
  , "Evaluation" ~: Evaluating.tests
  , "Executing" ~: Executing.tests
  , "EK Ast Show" ~: EKAstShowing.tests
  , "EK Parsing" ~: EKParsing.tests
  , "EK Typing" ~: EKTyping.tests
  ]

main :: IO ()
main = runTestTTAndExit tests
