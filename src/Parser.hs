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
  , parseInt
  , parseUInt
  , spaces
  , parseList
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

type Parser' a = String -> Either ParserError (a, String)
newtype Parser a = Parser { runParser :: Parser' a }

parseOneIf :: (Char -> Bool) -> Parser' Char
parseOneIf _ [] = Left "found EOF"
parseOneIf predicate (x:xs)
  | predicate x = Right (x, xs)
  | otherwise   = Left $ "found '" ++ [x] ++ "'"

parseChar :: Char -> Parser Char
parseChar expected
  = printf "Expected '%c' but %s" expected
    `mapError` Parser (parseOneIf (== expected))

parseAnyChar :: String -> Parser Char
parseAnyChar allowed
  = printf "Expected one of '%s' but %s" allowed
    `mapError` Parser (parseOneIf (`elem` allowed))

parseUInt :: Parser Integer
parseUInt = read <$> some (parseAnyChar ['0'..'9'])

parseInt :: Parser Integer
parseInt = (parseChar '-' >> negate <$> parseUInt) <|> parseUInt

spaces :: Parser [Char]
spaces = many $ parseChar ' '

parseList :: Parser a -> Parser [a]
parseList p = parseChar '(' *> many (spaces >> p) <* parseChar ')'

instance Functor Parser where
  fmap fct p = Parser $ runParser p >=> Right . mapFst fct
    where mapFst f (x, y) = (f x, y)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (x, input)
  liftA2 fct p1 p2 = p1 >>= \x -> fct x <$> p2

instance Alternative Parser where
  empty = Parser $ \_ -> Left "Empty parser"
  p1 <|> p2 = Parser $ \input -> runParser p1 input <> runParser p2 input

instance Monad Parser where
  p >>= fct = Parser $ runParser p >=> \(a, rest) -> runParser (fct a) rest

instance MonadFail Parser where
  fail err = Parser $ \_ -> Left err

instance MonadPlus Parser where
  -- default

mapError :: (ParserError -> ParserError) -> Parser a -> Parser a
mapError fct p = Parser $ \input -> mapError' (runParser p input)
  where mapError' (Left err) = Left $ fct err
        mapError' ok         = ok
