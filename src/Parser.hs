{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Parser
-}

module Parser
  ( Parser(..)
  , runParser
  , parseOneIf
  , parseChar
  , parseAnyChar
  , parseAnyButChar
  , parseAny
  , parseInt
  , parseUInt
  , spaces
  , parseList
  , parseString
  , parseNot
  , mapError
  , some
  , many
  , optional
  , getPos
  , (<|>)
  ) where

import Control.Applicative
  ( Applicative(liftA2)
  , Alternative((<|>), some, many, empty)
  , optional
  )
import Control.Monad(MonadPlus)
import Text.Printf (printf)

import SourcePos (SourcePos(..), Parsable(..), advance)

type ParserError = String

type Parser' inp out = SourcePos -> [inp] -> Either ParserError (out, [inp], SourcePos)
newtype Parser inp out  = Parser {runParser' :: Parser' inp out}

getPos :: Parser inp SourcePos
getPos = Parser $ \sourcePos input -> Right (sourcePos, input, sourcePos)

runParser :: Parser inp out -> [inp] -> Either ParserError (out, [inp])
runParser (Parser p) input = p (SourcePos "" 1 1) input >>= \(x, rest, _) -> Right (x, rest)

parseOneIf' :: (Show inp, Parsable inp) => (inp -> Bool) -> Parser' inp inp
parseOneIf'  _ sourcePos [] = Left ("found EOF at" ++ show sourcePos)
parseOneIf' predicate sourcePos (x:xs)
  | predicate x = Right (x, xs, advance sourcePos x)
  | otherwise   = Left $ "found " ++ show x

parseOneIf :: (Show inp, Parsable inp) => (inp -> Bool) -> Parser inp inp
parseOneIf predicate = Parser $ parseOneIf' predicate

parseChar :: Char -> Parser Char Char
parseChar expected
  = printf "Expected '%c' but %s" expected
    `mapError` parseOneIf (== expected)

parseAnyButChar :: Char -> Parser Char Char
parseAnyButChar expected
  = printf "Unexpected '%c'" expected
    `mapError` parseOneIf (/= expected)

parseAnyChar :: [Char] -> Parser Char Char
parseAnyChar allowed
  = printf "Expected one of '%s' but %s" allowed
    `mapError` parseOneIf (`elem` allowed)

parseAny :: Parser Char Char
parseAny = parseOneIf $ const True

parseUInt :: Parser Char Integer
parseUInt = read <$> some (parseAnyChar ['0'..'9'])

parseInt :: Parser Char Integer
parseInt = (parseChar '-' >> negate <$> parseUInt) <|> parseUInt

spaces :: Parser Char [Char]
spaces = many $ parseAnyChar " \t\n"

parseList :: Parser Char out -> Parser Char [out]
parseList p = parseChar '(' *> many (spaces >> p) <* spaces <* parseChar ')'

parseString :: Parser Char [Char]
parseString = parseChar '"' *> many (parseAnyButChar '"') <* parseChar '"'

parseNot :: Parser inp out -> Parser inp ()
parseNot p = Parser $ \sourcePos input -> parseNot' sourcePos input (runParser' p sourcePos input)
  where parseNot' sourcePos input (Left _) = Right ((), input, sourcePos)
        parseNot' _ _ _            = Left "Unexpected token"

instance Functor (Parser inp) where
  fmap fct p = Parser $ \sourcePos input -> fmap' (runParser' p sourcePos input)
    where fmap' (Left err) = Left err
          fmap' (Right (x, rest, sourcePos')) = Right (fct x, rest, sourcePos')

instance Applicative (Parser inp) where
  pure x = Parser $ \sourcePos input -> Right (x, input, sourcePos)
  liftA2 fct p1 p2 = p1 >>= \x -> fct x <$> p2

instance Alternative (Parser inp) where
  empty = Parser $ \_ _ -> Left "Empty parser"
  p1 <|> p2 = Parser $ \sourcePos input -> runParser' p1 sourcePos input <> runParser' p2 sourcePos input

instance Monad (Parser inp) where
  p >>= fct = Parser $ \sourcePos input -> runParser' p sourcePos input >>= \(x, rest, sourcePos') -> runParser' (fct x) sourcePos' rest

instance MonadFail (Parser inp) where
  fail err = Parser $ \_ _ -> Left err

instance MonadPlus (Parser inp) where
  -- default

mapError :: (ParserError -> ParserError) -> Parser inp out  -> Parser inp out
mapError fct p = Parser $ \sourcePos input -> mapError' (runParser' p sourcePos input)
  where mapError' (Left err) = Left $ fct err
        mapError' ok         = ok
