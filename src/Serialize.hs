{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
--
--}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Serialize
    ( saveResult
    , writeFunc
    , Serializable(..)
    ) where

import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import VirtualMachine
import Data.String(IsString(..))
import EK.Compiler(Result)
import Parser
import Data.Word

class Serializable a where
  serialize :: a -> B.ByteString
  deserialize :: Parser Word8 a

writeFunc :: (String, Insts) -> String -> IO ()
writeFunc (key, insts) path =
    B.writeFile (path ++ ".eko") (fromString key <> B.singleton 0 <>
    B.concat (fmap serialize insts) <> B.singleton 0)

saveResult :: Result -> String -> IO ()
saveResult result path =
    B.writeFile (path ++ ".eko") (B.singleton 42) >>
    mapM_ (\pair -> writeFunc pair path) (Map.toList result)

instance Serializable Integer where
  serialize integer = fromString (show integer) <> B.singleton 0
  deserialize = (read . fmap BI.w2c) <$> many (parseOneIf (/= 0))

instance Serializable Int where
  serialize int = fromString (show int) <> B.singleton 0
  deserialize = (read . fmap BI.w2c) <$> many (parseOneIf (/= 0)) <* parseOneIf (== 0)

instance Serializable String where
  serialize str = fromString str <> B.singleton 0
  deserialize = fmap BI.w2c <$> many (parseOneIf (/= 0)) <* parseOneIf (== 0)

instance Serializable VMValue where
  serialize (IntegerValue integer) = B.singleton 1 <> serialize integer
  serialize (AtomValue atom) = B.singleton 2 <> serialize atom
  serialize (StringValue str) = B.singleton 3 <> serialize str
  serialize (FunctionValue _) = B.singleton 0

  deserialize = parseOneIf (== 1) *> (IntegerValue <$> deserialize)
            <|> parseOneIf (== 2) *> (AtomValue <$> deserialize)
            <|> parseOneIf (== 3) *> (StringValue <$> deserialize)

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

  deserialize = parseOneIf (== 1) *> (Push <$> deserialize)
            <|> parseOneIf (== 2) *> pure Call
            <|> parseOneIf (== 3) *> pure (CallOp Add)
            <|> parseOneIf (== 4) *> pure (CallOp Sub)
            <|> parseOneIf (== 5) *> pure (CallOp Mul)
            <|> parseOneIf (== 6) *> pure (CallOp Div)
            <|> parseOneIf (== 7) *> pure (CallOp Eq)
            <|> parseOneIf (== 8) *> pure (CallOp Less)
            <|> parseOneIf (== 9) *> (JmpFalse <$> deserialize)
            <|> parseOneIf (== 10) *> pure Dup
            <|> parseOneIf (== 11) *> pure Ret
            <|> parseOneIf (== 12) *> (LoadArg <$> deserialize)
            <|> parseOneIf (== 13) *> (GetEnv <$> deserialize)
            <|> parseOneIf (== 14) *> pure (CallOp Print)
