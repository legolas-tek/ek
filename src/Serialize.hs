{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
--
--}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.Functor (($>))
import Control.Applicative (liftA2)
import Text.Printf (printf)
import System.Directory (getPermissions, setPermissions, setOwnerExecutable)
import EK.Types
import qualified Data.Range as Range

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

  deserialize = read . fmap BI.w2c <$> many (parseOneIf (/= 0)) <* exact 0

instance Serializable Int where
  serialize int = fromString (show int) <> B.singleton 0

  deserialize = read . fmap BI.w2c <$> many (parseOneIf (/= 0)) <* exact 0

instance Serializable Double where
  serialize float = fromString (show float) <> B.singleton 0

  deserialize = read . fmap BI.w2c <$> many (parseOneIf (/= 0)) <* exact 0

instance Serializable String where
  serialize str = fromString str <> B.singleton 0

  deserialize = fmap BI.w2c <$> many (parseOneIf (/= 0)) <* exact 0

instance Serializable VMValue where
  serialize (IntegerValue integer) = B.singleton 1 <> serialize integer
  serialize (AtomValue atom) = B.singleton 2 <> serialize atom
  serialize (StringValue str) = B.singleton 3 <> serialize str
  serialize (FloatValue float) = B.singleton 4 <> serialize float
  serialize (FunctionValue _) = B.singleton 0
  serialize (ClosureValue _ _) = B.singleton 0
  serialize (StructValue _ _) = B.singleton 0

  deserialize = exact 1 *> (IntegerValue <$> deserialize)
            <|> exact 2 *> (AtomValue <$> deserialize)
            <|> exact 3 *> (StringValue <$> deserialize)
            <|> exact 4 *> (FloatValue <$> deserialize)

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
  serialize (Construct name count) = B.singleton 20 <> serialize name <> serialize count
  serialize (Extract count) = B.singleton 21 <> serialize count
  serialize TailCall = B.singleton 22
  serialize (CheckConvertible ty) = B.singleton 23 <> serialize ty
  serialize (CallOp Concat) = B.singleton 24
  serialize (CallOp ToInt) = B.singleton 25
  serialize (CallOp ToFloat) = B.singleton 26
  serialize (CallOp CharAt) = B.singleton 27
  serialize (CallOp ToChar) = B.singleton 28
  serialize (CallOp ToCodePoint) = B.singleton 29

  deserialize = exact 1 *> (Push <$> deserialize)
            <|> exact 2 $> Call
            <|> exact 3 $> CallOp Add
            <|> exact 4 $> CallOp Sub
            <|> exact 5 $> CallOp Mul
            <|> exact 6 $> CallOp Div
            <|> exact 7 $> CallOp Eq
            <|> exact 8 $> CallOp Less
            <|> exact 9 *> (JmpFalse <$> (word8ToInt <$> parseOneIf (const True)))
            <|> exact 10 $> Dup
            <|> exact 11 $> Ret
            <|> exact 12 *> (LoadArg <$> (word8ToInt <$> parseOneIf (const True)))
            <|> exact 13 *> (GetEnv <$> deserialize)
            <|> exact 14 $> CallOp Print
            <|> exact 15 $> CallOp Exit
            <|> exact 16 *> (Closure <$> (word8ToInt <$> parseOneIf (const True)))
            <|> exact 17 $> CallOp EPrint
            <|> exact 18 $> CallOp ReadLine
            <|> exact 19 $> CallOp ToString
            <|> exact 20 *> (Construct <$> deserialize <*> deserialize)
            <|> exact 21 *> (Extract <$> deserialize)
            <|> exact 22 $> TailCall
            <|> exact 23 *> (CheckConvertible <$> deserialize)
            <|> exact 24 $> CallOp Concat
            <|> exact 25 $> CallOp ToInt
            <|> exact 26 $> CallOp ToFloat
            <|> exact 27 $> CallOp CharAt
            <|> exact 28 $> CallOp ToChar
            <|> exact 29 $> CallOp ToCodePoint

instance Serializable Type where
  serialize AnyTy = B.singleton 0
  serialize (UnionTy tys) = B.singleton 1 <> serialize tys
  serialize UnresolvedTy = B.singleton 2

  deserialize = exact 0 $> AnyTy
            <|> exact 1 *> (UnionTy <$> deserialize)
            <|> exact 2 $> UnresolvedTy

instance Serializable UnionType where
  serialize (UnionType {..}) = serialize atoms <> serialize functions <> serialize ints <> serialize structs

  deserialize = UnionType <$> deserialize <*> deserialize <*> deserialize <*> deserialize

instance Serializable [String] where
  serialize strs = B.concat (fmap ((B.singleton 1 <>) . serialize) strs) <> B.singleton 0

  deserialize = many (exact 1 *> deserialize) <* exact 0

instance (Serializable a, Serializable b) => Serializable [(a, b)] where
  serialize pairs = B.concat (fmap ((B.singleton 1 <>) . serialize) pairs) <> B.singleton 0

  deserialize = many (exact 1 *> deserialize) <* exact 0

instance Serializable (Range.Range Integer) where
  serialize (Range.SingletonRange value) = B.singleton 1 <> serialize value
  serialize (Range.SpanRange from to) = B.singleton 2 <> serialize (normalizeLBound from) <> serialize (normalizeUBound to)
  serialize (Range.LowerBoundRange l) = B.singleton 3 <> serialize (normalizeLBound l)
  serialize (Range.UpperBoundRange u) = B.singleton 4 <> serialize (normalizeUBound u)
  serialize Range.InfiniteRange = B.singleton 5

  deserialize = exact 1 *> (Range.SingletonRange <$> deserialize)
            <|> exact 2 *> (Range.SpanRange . inc <$> deserialize <*> (inc <$> deserialize))
            <|> exact 3 *> (Range.LowerBoundRange . inc <$> deserialize)
            <|> exact 4 *> (Range.UpperBoundRange . inc <$> deserialize)
            <|> exact 5 $> Range.InfiniteRange
                where inc b = Range.Bound b Range.Inclusive

instance Serializable [Range.Range Integer] where
  serialize bounds = B.concat (fmap serialize bounds) <> B.singleton 0

  deserialize = many deserialize <* exact 0

instance Serializable [Instruction] where
  serialize insts = B.concat (fmap serialize insts) <> B.singleton 0

  deserialize = many deserialize <* exact 0

instance (Serializable a, Serializable b) => Serializable (a, b) where
  serialize (key, insts) = serialize key <> serialize insts

  deserialize = liftA2 (,) deserialize deserialize

instance Serializable Result where
  serialize result = "#!/usr/bin/env ek\n" <> B.concat (fmap serialize (Map.toList result))

  deserialize = string (stringToWord8 "#!/usr/bin/env ek\n") *> (Map.fromList <$> many deserialize)

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
