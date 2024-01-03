{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
--
--}

{-# LANGUAGE OverloadedStrings #-}

module Serialize (serialize) where

import VirtualMachine
import qualified Data.ByteString as B
import Data.String(IsString(..))

serialize :: Instruction -> B.ByteString
serialize (Push value) = B.singleton 1 <> serializeVMValue value
serialize Call = B.singleton 2
serialize (CallOp Add) = B.singleton 3
serialize (CallOp Sub) = B.singleton 4
serialize (CallOp Mul) = B.singleton 5
serialize (CallOp Div) = B.singleton 6
serialize (CallOp Eq) = B.singleton 7
serialize (CallOp Less) = B.singleton 8
serialize (JmpFalse jmpValue) = B.singleton 9 <> B.singleton (fromIntegral jmpValue)
serialize Dup = B.singleton 10
serialize Ret = B.singleton 11
serialize (LoadArg value) = B.singleton 12 <> B.singleton (fromIntegral value)
serialize (PopEnv value) = B.singleton 13 <> fromString value

serializeVMValue :: VMValue -> B.ByteString
serializeVMValue (IntegerValue value) = B.singleton 1 <> fromString (show value)
serializeVMValue (AtomValue value) = B.singleton 2 <> fromString value
serializeVMValue (StringValue value) = B.singleton 3 <> fromString value
serializeVMValue (FunctionValue _) = B.singleton 0
