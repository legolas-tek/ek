{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

module EKResolving (tests) where

import Test.HUnit

import EK.Resolver
import EK.Types hiding (UnionType)
import EK.Ast

tests :: Test
tests = test
  [ "bool" ~: do
      let total = [ AtomDef "true"
                  , AtomDef "false"
                  , TypeDef "bool" (UnionType (TypeName "true") (TypeName "false"))
                  ]

      let typed = [ AtomDef "true"
                  , AtomDef "false"
                  , TypeDef "bool" (atomTy "true" <> atomTy "false")
                  ]
      resolveTypes total @?= (typed, [])
  , "fn" ~: do
      let total = [ FuncDef (FuncPattern [ArgPattern False "x" (Just $ IntRange Nothing Nothing)] (Just $ IntRange (Just 0) Nothing) Nothing) (IntegerLit 3)
                  ]

      let typed = [ FuncDef (FuncPattern [ArgPattern False "x" (Just intRangeInfTy)] (Just $ intRangeFromTy 0) Nothing) (IntegerLit 3)
                  ]
      resolveTypes total @?= (typed, [])
  ]
