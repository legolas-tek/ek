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
      let false = atomTy "false"
      let true = atomTy "true"
      let bool = false <> true
      show bool @?= "false | true"
      show false @?= "false"
      show true @?= "true"
  , "any" ~: do
      show AnyTy @?= "Any"
      show (AnyTy <> atomTy "false") @?= "Any"
      show (atomTy "false" <> AnyTy) @?= "Any"
  , "function" ~: do
      let f = functionTy (atomTy "false") (atomTy "true")
      show f @?= "(false -> true)"
  , "struct" ~: do
      let s = structTy "foo" [Field "bar" (atomTy "false")]
      show s @?= "foo"
  , "merge" ~: do
      let bool = atomTy "false" <> atomTy "true"
      let tribool = bool <> atomTy "undefined"
      show tribool @?= "false | true | undefined"
      let tribool2 = bool <> tribool
      tribool2 @?= tribool
      let complex = mconcat [bool, tribool, atomTy "nil", bool]
      show complex @?= "false | nil | true | undefined"
      stimes (3 :: Integer) bool @?= bool
      stimes (0 :: Integer) tribool @?= mempty
  , "ints" ~: do
      let c = intTy
      show (c 0 <> c 2) @?= "0 | 2"
      show (c 0 <> c 1) @?= "[0..1]"
      show (c 0 <> c 1 <> c 2 <> c (-1)) @?= "[-1..2]"
      show (c 0 <> c 1 <> c 2 <> c 27) @?= "[0..2] | 27"
  , "int ranges" ~: do
      show (intRangeTy 0 2) @?= "[0..2]"
      show (intRangeFromTy 0) @?= "[0..]"
      show (intRangeUpToTy 2) @?= "[..2]"
      show intRangeInfTy @?= "[..]"
  ]
