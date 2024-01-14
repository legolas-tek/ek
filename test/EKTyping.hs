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

bool :: Type
bool = atomTy "false" <> atomTy "true"

tests :: Test
tests = test
  [ "bool" ~: do
      let false = atomTy "false"
      let true = atomTy "true"
      show bool @?= "false | true"
      show false @?= "false"
      show true @?= "true"
  , "any" ~: do
      show AnyTy @?= "any"
      show (AnyTy <> atomTy "false") @?= "any"
      show (atomTy "false" <> AnyTy) @?= "any"
  , "function" ~: do
      let f = functionTy (atomTy "false") (atomTy "true")
      show f @?= "(false -> true)"
  , "struct" ~: do
      let s = structTy "foo"
      show s @?= "foo"
  , "merge" ~: do
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
      intTy 0 <> intTy 1 @?= intRangeTy 0 1
  , "atom convertibility" ~: do
      convertible (atomTy "false") (atomTy "false") @?= True
      convertible (atomTy "false") (atomTy "true") @?= False
      convertible (atomTy "false") AnyTy @?= True
      convertible AnyTy (atomTy "false") @?= False
      convertible (atomTy "false") bool @?= True
      convertible bool (atomTy "false") @?= False
      convertible bool bool @?= True
  , "range convertibility" ~: do
      convertible (intRangeTy 0 1) (intRangeTy 0 1) @?= True
      convertible (intRangeTy 0 1) (intRangeTy 0 2) @?= True
      convertible (intRangeTy 0 2) (intRangeTy 0 1) @?= False
      convertible (intRangeTy 0 1) (intRangeTy 1 2) @?= False
      convertible (intTy 0) (intRangeTy 0 1) @?= True
      convertible (intRangeTy 0 1) (intTy 0) @?= False
      convertible (intTy 0) (intTy 0) @?= True
      convertible (intTy 0) (intTy 1) @?= False
      convertible (intRangeTy 0 1) (intTy 1) @?= False
      convertible (intTy 1) (intRangeTy 0 1) @?= True
      convertible (intTy 2) (intRangeTy 0 1) @?= False
      convertible (intTy 2347) (intRangeUpToTy 42) @?= False
      convertible (intTy 2347) (intRangeFromTy 42) @?= True
      convertible (intTy 2347) intRangeInfTy @?= True
      convertible (atomTy "hey") intRangeInfTy @?= False
      convertible intRangeInfTy (atomTy "hey") @?= False
      convertible intRangeInfTy intRangeInfTy @?= True
      convertible mempty intRangeInfTy @?= True
      convertible intRangeInfTy mempty @?= False
  , "function convertibility" ~: do
      let ftob = functionTy (atomTy "false") bool
      let btob = functionTy bool bool
      convertible ftob ftob @?= True
      convertible ftob (functionTy (atomTy "false") (atomTy "true")) @?= False
      convertible (functionTy (atomTy "false") (atomTy "true")) ftob @?= True
      convertible btob ftob @?= True
      convertible ftob btob @?= False
      convertible btob btob @?= True
  , "struct convertibility" ~: do
      let s1 = structTy "BoolWrapper"
      let s2 = structTy "Flag"
      convertible s1 s1 @?= True
      convertible s1 s2 @?= False
      convertible s2 s1 @?= False
      convertible s2 s2 @?= True
      convertible s1 (s1 <> s2) @?= True
      convertible s2 (s1 <> s2) @?= True
      convertible (s1 <> s2) s1 @?= False
      convertible (s1 <> s2) (s1 <> s2) @?= True
  ]
