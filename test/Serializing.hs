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

tests :: Test
tests = test
  [ "serialize" ~: do
      serialize (Push (IntegerValue 49)) @?= "\1\SOH49\0"
      serialize (Push (AtomValue "test")) @?= "\1\2test\0"
      serialize (Push (StringValue "test str")) @?= "\1\3test str\0"
      serialize (Push (FunctionValue [Ret])) @?= "\1\0"
      serialize Call @?= "\2"
      serialize (CallOp Add)  @?= "\3"
      serialize (CallOp Sub)  @?= "\4"
      serialize (CallOp Mul)  @?= "\5"
      serialize (CallOp Div)  @?= "\6"
      serialize (CallOp Eq)  @?= "\7"
      serialize (CallOp Less)  @?= "\8"
      serialize (JmpFalse 49)  @?= "\t1"
      serialize (CallOp Print) @?= "\14"
      serialize Dup @?= "\10"
      serialize Ret @?= "\11"
      serialize (LoadArg 49) @?= "\f1"
      serialize (GetEnv "test") @?= "\13test\0"
      serialize ("test" :: String) @?= "test\0"
      serialize (42 :: Integer) @?= "42\0"
      serialize (42 :: Int) @?= "42\0"
  , "deserialize" ~: do
      let str = string (stringToWord8 "test")

      runParser str (stringToWord8 "test") @?= Right ((stringToWord8 "test"), (stringToWord8 ""))
  ]
