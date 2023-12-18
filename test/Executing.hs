{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

module Executing (tests) where

import Test.HUnit

import VirtualMachine

tests :: Test
tests = test
  [ "const" ~: do
      exec [Push $ IntegerValue 32, Ret] [] @?= Right [IntegerValue 32]
      exec [Push $ IntegerValue 10, Push $ IntegerValue 52, CallOp Sub, Ret] [] @?= Right [IntegerValue 42]
  ]
