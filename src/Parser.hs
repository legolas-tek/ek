{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- parser
-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parser where
import Control.Monad ( (>=>) )
import Control.Applicative (Applicative(liftA2))

type ParserError = String

type Parser' a = String -> Either ParserError (a, String)

parseChar :: Char -> Parser' Char
parseChar expected [] = Left $ "Expected '" ++ [expected] ++ "' but found EOF"
parseChar expected (x:xs)
    | x == expected = Right (x, xs)
    | otherwise     = Left $ "Expected '" ++ [expected] ++ "' but found '" ++ [x] ++ "'"

parseAnyChar :: String -> Parser' Char
parseAnyChar allowed []
  = Left $ "Expected one of '" ++ allowed ++ "' but found EOF"
parseAnyChar allowed (x:xs)
  | x `elem` allowed = Right (x, xs)
  | otherwise        = Left $ "Expected one of '" ++ allowed ++ "' but found '" ++ [x] ++ "'"

parseOr :: Parser' a -> Parser' a -> Parser' a
parseOr p1 p2 input = p1 input `combine` p2 input
  where
    combine (Left _) (Right ok) = Right ok
    combine (Right ok) _        = Right ok
    combine (Left err1) (Left err2) = Left $ err1 ++ " or " ++ err2

parseAnd :: Parser' a -> Parser' b -> Parser' (a, b)
parseAnd p1 p2 input = p1 input >>= applyNext
  where applyNext (ok1, rest) = p2 rest >>= finish ok1
        finish ok1 (ok2, rest) = Right ((ok1, ok2), rest)

parseAndWith :: (a -> b -> c) -> Parser' a -> Parser' b -> Parser' c
parseAndWith f p1 p2 input = parseAnd p1 p2 input >>= apply
  where apply ((ok1, ok2), rest) = Right (f ok1 ok2, rest)

parseMany :: Parser' a -> Parser' [a]
parseMany p = parseOr (parseAndWith (:) p (parseMany p)) parseZero
  where parseZero input = Right ([], input)

parseSome :: Parser' a -> Parser' [a]
parseSome p = parseAndWith (:) p (parseMany p)

parseUInt :: Parser' Integer
parseUInt input = parseSome (parseAnyChar ['0'..'9']) input >>= convert
  where convert (digits, rest) = Right (read digits, rest)

parseInt :: Parser' Integer
parseInt = parseOr parseNegative parseUInt
  where parseNegative = parseAndWith negate (parseChar '-') parseUInt
        negate _ i = -i

parsePair :: Parser' a -> Parser' (a, a)
parsePair p input = do
  (_, next) <- parseChar '(' input
  (x, next) <- p next
  (_, next) <- parseMany (parseChar ' ') next
  (y, next) <- p next
  (_, next) <- parseChar ')' next
  return ((x, y), next)

parseList :: Parser' a -> Parser' [a]
parseList p input = do
  (_, next) <- parseChar '(' input
  (x, next) <- parseSeparatedValues (parseMany (parseChar ' ')) p next
  (_, next) <- parseChar ')' next
  return (x, next)

-- Parse a list of values separated by a separator, without the parenthesis
parseSeparatedValues :: Parser' b -> Parser' a -> Parser' [a]
parseSeparatedValues sep p = parseAndWith sec sep (parseOr parseNonEmpty parseEmpty)
  where parseEmpty next = Right ([], next)
        parseNonEmpty = parseAndWith (:) p (parseSeparatedValues sep p)

sec :: a -> b -> b
sec _ b = b

newtype Parser a = Parser { runParser :: Parser' a }

instance Functor Parser where
  fmap fct p = Parser $ runParser p >=> Right . mapFst fct
    where mapFst f (x, y) = (f x, y)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (x, input)
  liftA2 fct p1 p2 = Parser $ parseAndWith fct (runParser p1) (runParser p2)

