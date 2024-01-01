{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- parser
-}

module Parser
  ( Parser(..)
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
  , (<|>)
  ) where

import Control.Applicative
  ( Applicative(liftA2)
  , Alternative((<|>), some, many, empty)
  )
import Control.Monad((>=>), MonadPlus)
import Text.Printf (printf)

type ParserError = String

type Parser' inp out = [inp] -> Either ParserError (out, [inp])
newtype Parser inp out  = Parser { runParser :: Parser' inp out }

parseOneIf :: (Char -> Bool) -> Parser' Char Char
parseOneIf _ [] = Left "found EOF"
parseOneIf predicate (x:xs)
  | predicate x = Right (x, xs)
  | otherwise   = Left $ "found '" ++ [x] ++ "'"

parseChar :: Char -> Parser Char Char
parseChar expected
  = printf "Expected '%c' but %s" expected
    `mapError` Parser (parseOneIf (== expected))

parseEscapedChar :: Parser Char Char
parseEscapedChar = parseChar '\\' *> parseChar '"'

parseAnyButChar :: Char -> Parser Char Char
parseAnyButChar expected
  = printf "Unexpected '%c'" expected
    `mapError` Parser (parseOneIf (/= expected))

parseAnyChar :: [Char] -> Parser Char Char
parseAnyChar allowed
  = printf "Expected one of '%s' but %s" allowed
    `mapError` Parser (parseOneIf (`elem` allowed))

parseAny :: Parser Char Char
parseAny = Parser $ parseOneIf $ const True

parseUInt :: Parser Char Integer
parseUInt = read <$> some (parseAnyChar ['0'..'9'])

parseInt :: Parser Char Integer
parseInt = (parseChar '-' >> negate <$> parseUInt) <|> parseUInt

spaces :: Parser Char [Char]
spaces = many $ parseAnyChar " \t\n"

parseList :: Parser Char out -> Parser Char [out]
parseList p = parseChar '(' *> many (spaces >> p) <* spaces <* parseChar ')'

parseString :: Parser Char String
parseString = parseChar '"' *> many (parseEscapedChar <|> parseAnyButChar '"') <* parseChar '"'

parseNot :: Parser Char out -> Parser Char ()
parseNot p = Parser $ \input -> parseNot' input (runParser p input)
  where parseNot' input (Left _) = Right ((), input)
        parseNot' _ _            = Left "Unexpected token"

instance Functor (Parser inp) where
  fmap fct p = Parser $ runParser p >=> Right . mapFst fct
    where mapFst f (x, y) = (f x, y)

instance Applicative (Parser inp) where
  pure x = Parser $ \input -> Right (x, input)
  liftA2 fct p1 p2 = p1 >>= \x -> fct x <$> p2

instance Alternative (Parser inp) where
  empty = Parser $ \_ -> Left "Empty parser"
  p1 <|> p2 = Parser $ \input -> runParser p1 input <> runParser p2 input

instance Monad (Parser inp) where
  p >>= fct = Parser $ runParser p >=> \(a, rest) -> runParser (fct a) rest

instance MonadFail (Parser inp) where
  fail err = Parser $ \_ -> Left err

instance MonadPlus (Parser inp) where
  -- default

mapError :: (ParserError -> ParserError) -> Parser inp out  -> Parser inp out
mapError fct p = Parser $ \input -> mapError' (runParser p input)
  where mapError' (Left err) = Left $ fct err
        mapError' ok         = ok
