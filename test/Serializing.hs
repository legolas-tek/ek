{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

{-# LANGUAGE OverloadedStrings #-}

module Serializing (tests) where

import Test.HUnit

import VirtualMachine

import Serialize

import qualified Data.ByteString as B

tests :: Test
tests = test
  [ "serialize" ~: do
    --   serialize Push (IntegerValue 3) @?= []
      serialize Ret @?= "\v"
  ]
