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
      let false = AtomTy "false"
      let true = AtomTy "true"
      let bool = concrete false <> concrete true
      show bool @?= "false | true"
      show (concrete false) @?= "false"
      show (concrete true) @?= "true"
  , "any" ~: do
      show AnyTy @?= "Any"
      show (AnyTy <> concrete (AtomTy "false")) @?= "Any"
      show (concrete (AtomTy "false") <> AnyTy) @?= "Any"
  , "function" ~: do
      let f = FunctionTy (concrete (AtomTy "false")) (concrete (AtomTy "true"))
      show f @?= "(false -> true)"
  , "struct" ~: do
      let s = StructTy "foo" [Field "bar" (concrete (AtomTy "false"))]
      show s @?= "foo"
  , "merge" ~: do
      let bool = concrete (AtomTy "false") <> concrete (AtomTy "true")
      let tribool = bool <> concrete (AtomTy "undefined")
      show tribool @?= "false | true | undefined"
      let tribool2 = bool <> tribool
      tribool2 @?= tribool
      let complex = mconcat [bool, tribool, concrete (AtomTy "nil"), bool]
      show complex @?= "false | nil | true | undefined"
      stimes (3 :: Integer) bool @?= bool
      stimes (0 :: Integer) tribool @?= mempty
  , "ints" ~: do
      let c = concrete . IntTy
      show (c 0 <> c 2) @?= "0 | 2"
      show (c 0 <> c 1) @?= "[0..1]"
      show (c 0 <> c 1 <> c 2 <> c (-1)) @?= "[-1..2]"
      show (c 0 <> c 1 <> c 2 <> c 27) @?= "[0..2] | 27"
  ]
