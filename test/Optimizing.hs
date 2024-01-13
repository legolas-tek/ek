{--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Optimizer
--}

module Optimizing (tests) where

import qualified Data.Map as Map
import VirtualMachine
import EK.Optimizer
import Test.HUnit

tests :: Test
tests = test
  [ "inline" ~: do
    let env = Map.fromList [("test", FunctionValue $ [Push $ (IntegerValue 5), Ret])]
    let insts = [Push $ (IntegerValue 5), GetEnv $ "test", Push $ (AtomValue "void"), Call]
    let res = Map.fromList [("first fn", insts), ("second fn", insts)]

    inlineInsts insts env @?=
      [Push $ (IntegerValue 5), Push $ (IntegerValue 5), Ret]
    inlineInsts insts (Map.fromList []) @?=
      insts
    inlineResult res env @?=
      Map.fromList [("first fn", [Push $ (IntegerValue 5), Push $ (IntegerValue 5), Ret])
      , ("second fn", [Push $ (IntegerValue 5), Push $ (IntegerValue 5), Ret])]
  ]
