{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
--
--}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Serialize
    ( saveResult
    , string
    , exact
    , stringToWord8
    , loadResult
    , Serializable(..)
    ) where

import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI

import Diagnostic
import VirtualMachine
import Parser

import Data.String(IsString(..))
import EK.Compiler(Result)
import Data.Word (Word8)
import Control.Applicative (liftA2)
import Text.Printf (printf)
import System.Directory (getPermissions, setPermissions, setOwnerExecutable)

type DeserializerError = Diagnostic

exact :: Word8 -> Parser Word8 Word8
exact expected
  = printf "Expected '%c' but %s" expected
    `mapError` parseOneIf (== expected)

string :: [Word8] -> Parser Word8 [Word8]
string expected
  = printf "Expected \"%s\" but %s" (map BI.w2c expected)
    `mapError` traverse exact expected

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral . toInteger

stringToWord8 :: String -> [Word8]
stringToWord8 str = B.unpack (BI.packChars str)

class Serializable a where
  serialize :: a -> B.ByteString
  deserialize :: Parser Word8 a

instance Serializable Integer where
  serialize integer = fromString (show integer) <> B.singleton 0

  deserialize = (read . fmap BI.w2c) <$> many (parseOneIf (/= 0)) <* parseOneIf (== 0)

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
  serialize (ClosureValue _ _) = B.singleton 0

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
  serialize (GetEnv value) = B.singleton 13 <> serialize value
  serialize (CallOp Print) = B.singleton 14
  serialize (CallOp Exit) = B.singleton 15
  serialize (Closure value) = B.singleton 16 <> B.singleton (fromIntegral value)
  serialize (CallOp EPrint) = B.singleton 17
  serialize (CallOp ReadLine) = B.singleton 18
  serialize (CallOp ToString) = B.singleton 19

  deserialize = parseOneIf (== 1) *> (Push <$> deserialize)
            <|> parseOneIf (== 2) *> pure Call
            <|> parseOneIf (== 3) *> pure (CallOp Add)
            <|> parseOneIf (== 4) *> pure (CallOp Sub)
            <|> parseOneIf (== 5) *> pure (CallOp Mul)
            <|> parseOneIf (== 6) *> pure (CallOp Div)
            <|> parseOneIf (== 7) *> pure (CallOp Eq)
            <|> parseOneIf (== 8) *> pure (CallOp Less)
            <|> parseOneIf (== 9) *> (JmpFalse <$> (word8ToInt <$> parseOneIf (const True)))
            <|> parseOneIf (== 10) *> pure Dup
            <|> parseOneIf (== 11) *> pure Ret
            <|> parseOneIf (== 12) *> (LoadArg <$> (word8ToInt <$> parseOneIf (const True)))
            <|> parseOneIf (== 13) *> (GetEnv <$> deserialize)
            <|> parseOneIf (== 14) *> pure (CallOp Print)
            <|> parseOneIf (== 15) *> pure (CallOp Exit)
            <|> parseOneIf (== 16) *> (Closure <$> (word8ToInt <$> parseOneIf (const True)))
            <|> parseOneIf (== 17) *> pure (CallOp EPrint)
            <|> parseOneIf (== 18) *> pure (CallOp ReadLine)
            <|> parseOneIf (== 19) *> pure (CallOp ToString)

instance Serializable [Instruction] where
  serialize insts = B.concat (fmap serialize insts) <> B.singleton 0

  deserialize = many deserialize <* parseOneIf (== 0)

instance (Serializable a, Serializable b) => Serializable (a, b) where
  serialize (key, insts) = serialize key <> serialize insts

  deserialize = liftA2 (,) deserialize deserialize

instance Serializable Result where
  serialize result = "#!/usr/bin/env ek\n" <> B.concat (fmap serialize (Map.toList result))

  deserialize = string (stringToWord8 "#!/usr/bin/env ek\n") *> (Map.fromList <$> (many deserialize))

setFileExecutable :: String -> IO ()
setFileExecutable path = getPermissions path >>= \perms ->
    setPermissions path (setOwnerExecutable True perms)

saveResult :: Result -> String -> IO ()
saveResult result path =
    B.writeFile path (serialize result) >> setFileExecutable path

resultParser :: Parser Word8 Result
resultParser = deserialize <* eof

getWord8List :: String -> IO [Word8]
getWord8List path = B.unpack <$> B.readFile path

loadResult :: String -> IO (Either DeserializerError Result)
loadResult path = (fst <$> ) . runParserOnFile resultParser path <$> getWord8List path