{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- argument parser
-}

{-# LANGUAGE FlexibleInstances #-}

module ArgParser
  ( Arguments(..)
  , OutputType(..)
  , parseArguments
  ) where

import Parser
import Diagnostic
import Data.Functor (($>))

type ArgParser a = Parser String a

data OutputType = OutputTokens | OutputAst | OutputTypedAst | OutputBytecode | OutputResult
  deriving (Eq)

data Arguments = Arguments
  { argInput :: Maybe String
  , argOutput :: Maybe String
  , argOutputType :: Maybe OutputType
  , argOptimize :: Bool
  , argImportPath :: [String]
  }

instance Semigroup Arguments where
  a <> b = Arguments
    { argInput = argInput a <|> argInput b
    , argOutput = argOutput a <|> argOutput b
    , argOutputType = argOutputType a <|> argOutputType b
    , argOptimize = argOptimize a || argOptimize b
    , argImportPath = argImportPath a <> argImportPath b
    }

instance Monoid Arguments where
  mempty = Arguments
    { argInput = Nothing
    , argOutput = Nothing
    , argOutputType = Nothing
    , argOptimize = False
    , argImportPath = []
    }

one :: ArgParser String
one = parseOneIf (const True)

parseExact :: String -> ArgParser String
parseExact = parseOneIf . (==)

input :: ArgParser String
input = one

output :: ArgParser String
output = parseExact "-o" *> one

outputTypeFlag :: ArgParser String
outputTypeFlag = parseExact "-t" <|> parseExact "--emit"

importPathFlag :: ArgParser String
importPathFlag = parseExact "-I" *> one

outputType :: ArgParser OutputType
outputType = (outputTypeFlag *> parseOneIf (== "tokens") $> OutputTokens)
         <|> (outputTypeFlag *> parseOneIf (== "ast") $> OutputAst)
         <|> (outputTypeFlag *> parseOneIf (== "typed-ast") $> OutputTypedAst)
         <|> (outputTypeFlag *> parseOneIf (== "bytecode") $> OutputBytecode)
         <|> (outputTypeFlag *> parseOneIf (== "result") $> OutputResult)

withArgOutput :: String -> Arguments
withArgOutput o = mempty { argOutput = Just o }

withArgOutputType :: OutputType -> Arguments
withArgOutputType t = mempty { argOutputType = Just t }

withImportPath :: String -> Arguments
withImportPath i = mempty { argImportPath = [i] }

withArgInput :: String -> Arguments
withArgInput i = mempty { argInput = Just i }

withArgOptimize :: Arguments
withArgOptimize = mempty { argOptimize = True }

argument :: ArgParser Arguments
argument = withArgOutput <$> output
       <|> withArgOutputType <$> outputType
       <|> withArgOptimize <$ parseExact "-O"
       <|> withImportPath <$> importPathFlag
       <|> withArgInput <$> input

arguments :: ArgParser Arguments
arguments = mconcat <$> many argument

parseArguments :: [String] -> Either Diagnostic Arguments
parseArguments args = fst <$> runParser arguments args
