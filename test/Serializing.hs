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

import Parser

import qualified Data.ByteString as B

tests :: Test
tests = test
  [ "serialize" ~: do
      serialize (Push (IntegerValue 49)) @?= "\SOH\SOH49\0"
      serialize (Push (AtomValue "test")) @?= "\SOH\STXtest\0"
      serialize (Push (StringValue "test str")) @?= "\SOH\ETXtest str\0"
      serialize (Push (FunctionValue [Ret])) @?= "\SOH\0"
      serialize Call @?= "\STX"
      serialize (CallOp Add)  @?= "\ETX"
      serialize (CallOp Sub)  @?= "\EOT"
      serialize (CallOp Mul)  @?= "\ENQ"
      serialize (CallOp Div)  @?= "\ACK"
      serialize (CallOp Eq)  @?= "\BEL"
      serialize (CallOp Less)  @?= "\BS"
      serialize (JmpFalse 49)  @?= "\t1"
      serialize (CallOp Print) @?= "\SO"
      serialize Dup @?= "\n"
      serialize Ret @?= "\v"
      serialize (LoadArg 49) @?= "\f1"
      serialize (GetEnv "test") @?= "\rtest\0"
      serialize ("test" :: String) @?= "test\0"
      serialize (42 :: Integer) @?= "42\0"
      serialize (42 :: Int) @?= "42\0"
--   , "deserialize" ~: do
--       let serializedStr = serialize "Test"

--       deserialize (B.unpack serializedStr) @?= Right "Test"
  ]
