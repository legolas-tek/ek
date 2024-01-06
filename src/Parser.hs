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
  , parseStringLit
  , parseString
  , parseNot
  , mapError
  , some
  , many
  , optional
  , getPos
  , eof
  , runParserOnFile
  , (<|>)
  ) where

import Control.Applicative
  ( Applicative(liftA2)
  , Alternative((<|>), some, many, empty)
  , optional
  )
import Control.Monad (MonadPlus)
import Data.Functor (($>))
import Text.Printf (printf)
import Diagnostic
import SourcePos

type ParserError = Diagnostic

type Parser' inp out = SourcePos -> [Diagnostic] -> [inp] -> Either ParserError (out, [inp], [Diagnostic], SourcePos)
newtype Parser inp out  = Parser {runParser' :: Parser' inp out}

getPos :: Parser inp SourcePos
getPos = Parser $ \sourcePos diagnostics input -> Right (sourcePos, input, diagnostics, sourcePos)

eof :: (Show inp, Parsable inp) => Parser inp ()
eof = parseNot (parseOneIf $ const True) <|> fail "Unexpected trailing token"

runParser :: Parser inp out -> [inp] -> Either ParserError (out, [inp])
runParser (Parser p) input = p (SourcePos "" 1 1) [] input >>= \(x, rest, _, _) -> Right (x, rest)

runParserOnFile :: Parser inp out -> String -> [inp] -> Either ParserError out
runParserOnFile (Parser p) fileName fileContent = p (SourcePos fileName 1 1) [] fileContent >>= \(x, _, _, _) -> Right x

parseOneIf' :: (Show inp, Parsable inp) => (inp -> Bool) -> Parser' inp inp
parseOneIf'  _ sourcePos _ [] = Left $ Diagnostic Error "found EOF" sourcePos
parseOneIf' predicate sourcePos diags (x:xs)
  | predicate x = Right (x, xs, diags, advance sourcePos x)
  | otherwise   = Left $ Diagnostic Error ("found " ++ show x) sourcePos

parseOneIf :: (Show inp, Parsable inp) => (inp -> Bool) -> Parser inp inp
parseOneIf predicate = Parser $ parseOneIf' predicate

parseChar :: Char -> Parser Char Char
parseChar expected
  = printf "Expected '%c' but %s" expected
    `mapError` parseOneIf (== expected)

parseString :: String -> Parser Char [Char]
parseString expected
  = printf "Expected \"%s\" but %s" expected
    `mapError` traverse parseChar expected

parseEscapedChar :: Parser Char Char
parseEscapedChar = parseChar '\\' *>
  (   (parseChar '0' $> '\0')
  <|> (parseChar '\\' $> '\\')
  <|> (parseChar '"' $> '"')
  <|> (parseChar 't' $> '\t')
  <|> (parseChar 'n' $> '\n')
  <|> (parseChar 'r' $> '\r')
  )

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

parseStringLit :: Parser Char String
parseStringLit = parseChar '"' *> many (parseEscapedChar <|> parseAnyButChar '"') <* parseChar '"'

parseNot :: Parser inp out -> Parser inp ()
parseNot p = Parser $ \sourcePos diags input -> parseNot' sourcePos [] input (runParser' p sourcePos diags input)
  where parseNot' sourcePos diags input (Left _) = Right ((), input, diags, sourcePos)
        parseNot' sourcePos _ _ _            = Left $ Diagnostic Error "Unexpected token" sourcePos

instance Functor (Parser inp) where
  fmap fct p = Parser $ \sourcePos diags input -> fmap' (runParser' p sourcePos diags input)
    where fmap' (Left err) = Left err
          fmap' (Right (x, rest, diags, sourcePos')) = Right (fct x, rest, diags, sourcePos')

instance Applicative (Parser inp) where
  pure x = Parser $ \sourcePos diags input -> Right (x, input, diags, sourcePos)
  liftA2 fct p1 p2 = p1 >>= \x -> fct x <$> p2

instance Alternative (Parser inp) where
  empty = Parser $ \sourcePos _ _ -> Left $ Diagnostic Error "Empty parser" sourcePos
  p1 <|> p2 = Parser $ \sourcePos input -> runParser' p1 sourcePos input <> runParser' p2 sourcePos input

instance Monad (Parser inp) where
  p >>= fct = Parser $ \sourcePos diags input -> runParser' p sourcePos diags input >>= \(x, rest, diags', sourcePos') -> runParser' (fct x) sourcePos' diags' rest

instance MonadFail (Parser inp) where
  fail err = Parser $ \sourcePos _ _ -> Left $ Diagnostic Error err sourcePos

instance MonadPlus (Parser inp) where
  -- default

mapError :: (String -> String) -> Parser inp out -> Parser inp out
mapError fct p = Parser $ \sourcePos diags input -> mapError' (runParser' p sourcePos diags input)
  where mapError' (Left diag) = Left $ diag { message = fct (message diag) }
        mapError' ok           = ok
