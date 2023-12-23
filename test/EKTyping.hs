{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

module EKTyping (tests) where

import Test.HUnit

import EK.Types
import Data.Semigroup (Semigroup(stimes))

tests :: Test
tests = test
  [ "bool" ~: do
      let false = Atom "false"
      let true = Atom "true"
      let bool = concrete false <> concrete true
      show bool @?= "false | true"
      show (concrete false) @?= "false"
      show (concrete true) @?= "true"
  , "any" ~: do
      show TAny @?= "Any"
      show (TAny <> concrete (Atom "false")) @?= "Any"
      show (concrete (Atom "false") <> TAny) @?= "Any"
  , "function" ~: do
      let f = Function (concrete (Atom "false")) (concrete (Atom "true"))
      show f @?= "(false -> true)"
  , "struct" ~: do
      let s = Struct "foo" [Field "bar" (concrete (Atom "false"))]
      show s @?= "foo"
  , "merge" ~: do
      let bool = concrete (Atom "false") <> concrete (Atom "true")
      let tribool = bool <> concrete (Atom "undefined")
      show tribool @?= "false | true | undefined"
      let tribool2 = bool <> tribool
      tribool2 @?= tribool
      let complex = mconcat [bool, tribool, concrete (Atom "nil"), bool]
      show complex @?= "false | nil | true | undefined"
      stimes (3 :: Integer) bool @?= bool
      stimes (0 :: Integer) tribool @?= mempty
  , "ints" ~: do
      let c = concrete . Int
      show (c 0 <> c 2) @?= "0 | 2"
      show (c 0 <> c 1) @?= "[0..1]"
      show (c 0 <> c 1 <> c 2 <> c (-1)) @?= "[-1..2]"
      show (c 0 <> c 1 <> c 2 <> c 27) @?= "[0..2] | 27"
  ]
