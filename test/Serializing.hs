{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

{-# LANGUAGE OverloadedStrings #-}

module Serializing (tests) where
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Test.HUnit
import VirtualMachine
import Serialize
import Parser
import Diagnostic
import SourcePos
import Data.Word
import EK.Compiler(Result)

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
      serialize (JmpFalse 49)  @?= "\9\49"
      serialize (CallOp Print) @?= "\14"
      serialize Dup @?= "\10"
      serialize Ret @?= "\11"
      serialize (LoadArg 49) @?= "\f1"
      serialize (GetEnv "test") @?= "\13test\0"
      serialize ("test" :: String) @?= "test\0"
      serialize (42 :: Integer) @?= "42\0"
      serialize (42 :: Int) @?= "42\0"

   , "string" ~: do
      let str = string (stringToWord8 "test")

      runParser str (stringToWord8 "test") @?= Right ((stringToWord8 "test"), (stringToWord8 ""))
      runParser str (stringToWord8 "test123") @?= Right ((stringToWord8 "test"), (stringToWord8 "123"))
      runParser str (stringToWord8 "123") @?= Left (Diagnostic Error "Expected \"test\" but Expected 't' but found 49" (SourcePos "" 1 1))

   , "deserialize str" ~: do
      let serializedStr = B.unpack (serialize ("test" :: String))

      runParser deserialize serializedStr @?= Right (("test" :: String), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 String) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found EOF" (SourcePos "" 1 12))

   , "deserialize int" ~: do
      let serializedInt = B.unpack (serialize (42 :: Int))

      runParser deserialize serializedInt @?= Right ((42 :: Int), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Int) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found EOF" (SourcePos "" 1 12))

   , "deserialize integer" ~: do
      let serializedInteger = B.unpack (serialize (42 :: Integer))

      runParser deserialize serializedInteger @?= Right ((42 :: Integer), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Integer) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found EOF" (SourcePos "" 1 12))

   , "deserialize VMValue" ~: do
      let serializedVMInteger = B.unpack (serialize (IntegerValue $ 42))
      let serializedVMAtomValue = B.unpack (serialize (AtomValue $ "test"))
      let serializedVMStringValue = B.unpack (serialize (StringValue $ "test"))

      runParser deserialize serializedVMInteger @?= Right ((IntegerValue $ 42), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 VMValue) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedVMAtomValue @?= Right ((AtomValue $ "test"), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 VMValue) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedVMStringValue @?= Right ((StringValue $ "test"), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 VMValue) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))

   , "deserialize instruction" ~: do
      let serializedPush = B.unpack (serialize (Push $ IntegerValue $ 42))
      let serializedCall = B.unpack (serialize Call)
      let serializedCallAdd = B.unpack (serialize (CallOp Add))
      let serializedCallSub = B.unpack (serialize (CallOp Sub))
      let serializedCallMul = B.unpack (serialize (CallOp Mul))
      let serializedCallDiv = B.unpack (serialize (CallOp Div))
      let serializedCallEq = B.unpack (serialize (CallOp Eq))
      let serializedCallLess = B.unpack (serialize (CallOp Less))
      let serializedCallPrint = B.unpack (serialize (CallOp Print))
      let serializedCallExit = B.unpack (serialize (CallOp Exit))
      let serializedJmpFalse = B.unpack (serialize (JmpFalse $ 42))
      let serializedDup = B.unpack (serialize Dup)
      let serializedRet = B.unpack (serialize Ret)
      let serializedLoadArg = B.unpack (serialize (LoadArg $ 42))
      let serializedGetEnv = B.unpack (serialize (GetEnv $ "test"))
      let serializedClosure = B.unpack (serialize (Closure $ 42))

      runParser deserialize serializedPush @?= Right ((Push $ IntegerValue $ 42), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedCall @?= Right (Call, (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedCallAdd @?= Right ((CallOp $ Add), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedCallSub @?= Right ((CallOp $ Sub), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedCallMul @?= Right ((CallOp $ Mul), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedCallDiv @?= Right ((CallOp $ Div), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedCallEq @?= Right ((CallOp $ Eq), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedCallLess @?= Right ((CallOp $ Less), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedCallPrint @?= Right ((CallOp $ Print), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedCallExit @?= Right ((CallOp $ Exit), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedJmpFalse @?= Right (JmpFalse $ 42, (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedDup @?= Right (Dup, (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedRet @?= Right (Ret, (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedLoadArg @?= Right ((LoadArg $ 42), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedGetEnv @?= Right ((GetEnv $ "test"), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))
      runParser deserialize serializedClosure @?= Right ((Closure $ 42), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Instruction) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))

   , "deserialize instruction list" ~: do
      let serializedInstructs = B.unpack (serialize [Push $ IntegerValue $ 42, Call, CallOp Add])

      runParser deserialize serializedInstructs @?= Right ([(Push $ IntegerValue $ 42), Call, (CallOp Add)], (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Insts) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found 102" (SourcePos "" 1 1))

   , "deserialize tuple" ~: do
      let serializedTuple = B.unpack $ serialize ("test" :: String, Call)

      runParser deserialize serializedTuple @?= Right (("test" :: String, Call), (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 (String, Instruction)) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "found EOF" (SourcePos "" 1 12))

   , "deserialize tuple" ~: do
      let serializedResult = B.unpack $ serialize $ Map.fromList [(("test1" :: String), [Call, Ret]), (("test2" :: String), [Dup, Ret])]

      runParser deserialize serializedResult @?= Right (Map.fromList $ [(("test1" :: String) , [Call, Ret]), (("test2" :: String), [Dup, Ret])], (stringToWord8 ""))
      runParser (deserialize :: Parser Word8 Result) (stringToWord8 "failurecase") @?= Left (Diagnostic Error "Expected \"#!/usr/bin/env ek\n\" but Expected '#' but found 102" (SourcePos "" 1 1))
  ]
