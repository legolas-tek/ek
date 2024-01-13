{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

import Test.HUnit

import qualified Parsing
import qualified Executing
import qualified EKAstShowing
import qualified EKParsing
import qualified EKTyping
import qualified Compilation
import qualified Tokenizing
import qualified Serializing
import qualified Optimizing

tests :: Test
tests = test
  [ "Parsing tests" ~: Parsing.tests
  , "Executing" ~: Executing.tests
  , "EK Ast Show" ~: EKAstShowing.tests
  , "EK Parsing" ~: EKParsing.tests
  , "EK Typing" ~: EKTyping.tests
  , "Compilation" ~: Compilation.tests
  , "Tokenize" ~: Tokenizing.tests
  , "Serializing" ~: Serializing.tests
  , "Optimizing" ~: Optimizing.tests
  ]

main :: IO ()
main = runTestTTAndExit tests
