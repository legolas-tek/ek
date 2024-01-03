{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

module Serializing (tests) where

import Test.HUnit
import Data.Either (isLeft)
import VirtualMachine

import Parser

tests :: Test
tests = test
  [ "serialize" ~: do
    --   serialize Push (IntegerValue 3) @?= []
      serialize Ret @?= [11]
  ]
