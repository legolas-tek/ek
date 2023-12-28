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
import qualified EKTyping
import qualified Compilation

tests :: Test
tests = test
  [ "Parsing tests" ~: Parsing.tests
  , "Lisp parsing" ~: LispParsing.tests
  , "Evaluation" ~: Evaluating.tests
  , "Executing" ~: Executing.tests
  , "EK Ast Show" ~: EKAstShowing.tests
  , "EK Typing" ~: EKTyping.tests
  , "Compilation" ~: Compilation.tests
  ]

main :: IO ()
main = runTestTTAndExit tests
