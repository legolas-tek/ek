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
    inlineResult res (Map.fromList []) @?=
      Map.fromList [("first fn", insts), ("second fn", insts)]
  , "delete useless functions" ~: do
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
   , "optimize bytecode" ~: do
      let result = Map.fromList
            [ ("add_first", [Push (IntegerValue 1), Push (IntegerValue 2), CallOp Add, Ret])
            , ("add_second", [Push (IntegerValue 1), Push (IntegerValue 2), CallOp Add, Ret])
            , ("main", [GetEnv "add_first", GetEnv "add_second"])
            ]
      let expected = Map.fromList
            [ ("add_first", [Push (IntegerValue 3), Ret])
            , ("main", [GetEnv "add_first", GetEnv "add_first"])
            ]
      expected @?= optimizeBytecode result
  ]
