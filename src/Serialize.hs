{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
--
--}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Serialize (serialize, saveResult, writeFunc) where

import qualified Data.Map as Map
import qualified Data.ByteString as B
import VirtualMachine
import Data.String(IsString(..))
import EK.Compiler(Result)

class Serializable a where
  serialize :: a -> B.ByteString

writeFunc :: (String, Insts) -> String -> IO ()
writeFunc (key, insts) path =
    B.writeFile (path ++ ".eko") (fromString key <> B.singleton 0 <>
    B.concat (fmap serialize insts) <> B.singleton 0)

saveResult :: Result -> String -> IO ()
saveResult result path =
    B.writeFile (path ++ ".eko") (B.singleton 42) >>
    mapM_ (\pair -> writeFunc pair path) (Map.toList result)

instance Serializable Instruction where
  serialize (Push value) = B.singleton 1 <> serialize value
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
  serialize (GetEnv value) = B.singleton 13 <> fromString value <> B.singleton 0
  serialize (CallOp Print) = B.singleton 14

instance Serializable Integer where
  serialize integer = fromString (show integer) <> B.singleton 0

instance Serializable Int where
  serialize int = fromString (show int) <> B.singleton 0

instance Serializable String where
  serialize str = fromString str <> B.singleton 0

instance Serializable VMValue where
  serialize (IntegerValue integer) = B.singleton 1 <> serialize integer
  serialize (AtomValue atom) = B.singleton 2 <> serialize atom
  serialize (StringValue str) = B.singleton 3 <> serialize str
  serialize (FunctionValue _) = B.singleton 0
