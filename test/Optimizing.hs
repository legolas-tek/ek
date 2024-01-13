--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Optimizing
--

module Optimizing (tests) where

import qualified Data.Map as Map (fromList)
import VirtualMachine
import EK.Optimizer
import Test.HUnit

tests :: Test
tests = test
  [ "delete useless functions" ~: do
      let result = Map.fromList
            [ ("main", [Push (IntegerValue 1), Push (IntegerValue 2), CallOp Add, Ret])
            , ("add", [Push (IntegerValue 1), Push (IntegerValue 2), CallOp Add, Ret])
            , ("sub", [Push (IntegerValue 1), Push (IntegerValue 2), CallOp Sub, Ret])
            ]
      let expected = Map.fromList
            [ ("main", [Push (IntegerValue 1), Push (IntegerValue 2), CallOp Add, Ret]) ]
      expected @?= deleteNotUsedFunc result
  , "merge functions with same instructions" ~: do
      let result = Map.fromList
            [ ("add_first", [Push (IntegerValue 1), Push (IntegerValue 2), CallOp Add, Ret])
            , ("add_second", [Push (IntegerValue 1), Push (IntegerValue 2), CallOp Add, Ret])
            , ("main", [GetEnv "add_first", GetEnv "add_second"])
            ]
      let expected = Map.fromList
            [ ("add_first", [Push (IntegerValue 1), Push (IntegerValue 2), CallOp Add, Ret])
            , ("add_second", [Push (IntegerValue 1), Push (IntegerValue 2), CallOp Add, Ret])
            , ("main", [GetEnv "add_first", GetEnv "add_first"])
            ]
      expected @?= deleteSameInstsOfFunc result
  , "optimize Insts" ~: do
      let instsAdd = [Push (IntegerValue 1), Push (IntegerValue 2), CallOp Add, Ret]
      let expectedAdd = [Push (IntegerValue 3), Ret]
      expectedAdd @?= optimizeInsts instsAdd
      let instsSub = [Push (IntegerValue 1), Push (IntegerValue 2), CallOp Sub, Ret]
      let expectedSub = [Push (IntegerValue (-1)) ,Ret]
      expectedSub @?= optimizeInsts instsSub
      let instsMul = [Push (IntegerValue 1), Push (IntegerValue 2), CallOp Mul, Ret]
      let expectedMul = [Push (IntegerValue 2), Ret]
      expectedMul @?= optimizeInsts instsMul
  ]
