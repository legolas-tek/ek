--
-- EPITECH PROJECT, 2023
-- B-FUN-500-PAR-5-2-glados-jeremy.elalouf
-- File description:
-- Token
--

module Token where

data TokenType = IntLiteral
               | String
               | Identifier
               deriving (Show)

data Token = Token { string  :: String
                   , line :: Int
                   , column :: Int
                   , fileName :: String
                   , tokenType :: TokenType
                   } deriving (Show)
