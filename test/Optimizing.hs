{--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Optimizing
--}

module Optimizing (tests) where

import qualified Data.Map as Map (fromList, empty)
import VirtualMachine
import EK.Optimizer
import Test.HUnit

tests :: Test
tests = test
  [ "convertInlinedInsts" ~: do
    let insts = [LoadArg 0, Push $ IntegerValue 5, CallOp Add, LoadArg 0, CallOp Sub, Ret]
    let insts' = [LoadArg 1, Push $ IntegerValue 5, CallOp Add, LoadArg 1, CallOp Sub, Ret]

    convertInlinedInsts insts (IntegerValue 10) @?=
      [Push $ IntegerValue 10, Push $ IntegerValue 5, CallOp Add, Push $ IntegerValue 10, CallOp Sub]
    convertInlinedInsts insts' (IntegerValue 10) @?=
      [LoadArg 1, Push $ IntegerValue 5, CallOp Add, LoadArg 1, CallOp Sub]

  , "inline" ~: do
    let env = Map.fromList [("test", [Push $ IntegerValue 5, Ret])]
    let insts = [Push $ IntegerValue 5, GetEnv $ "test", Push $ AtomValue "void", Call]
    let res = Map.fromList [("first fn", insts), ("second fn", insts)
              , ("test", [Push $ IntegerValue 5])]
    let recuRes = Map.fromList [("first fn", insts)
                  , ("second fn", [GetEnv $ "first fn", Push $ AtomValue "void", Call])
                  , ("test", [Push $ IntegerValue 5])]
    let recuRes' = Map.fromList [("first fn", [GetEnv $ "first fn", Push $ AtomValue "void", Call])]

    inlineInsts insts env "foo" @?=
      [Push $ IntegerValue 5, Push $ IntegerValue 5]
    inlineInsts insts Map.empty "foo" @?=
      insts
    inlineResult res @?=
      Map.fromList [("first fn", [Push $ IntegerValue 5, Push $ IntegerValue 5])
      , ("second fn", [Push $ IntegerValue 5, Push $ IntegerValue 5])
      , ("test", [Push $ IntegerValue 5])]
    inlineResult recuRes @?=
      Map.fromList [("first fn", [Push $ IntegerValue 5, Push $ IntegerValue 5])
      , ("second fn", [Push $ IntegerValue 5, Push $ IntegerValue 5])
      , ("test", [Push $ IntegerValue 5])]
    inlineResult recuRes' @?=
      Map.fromList [("first fn", [GetEnv $ "first fn", Push $ AtomValue "void", Call])]

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
