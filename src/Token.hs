--
-- EPITECH PROJECT, 2023
-- B-FUN-500-PAR-5-2-glados-jeremy.elalouf
-- File description:
-- Token
--

module Token where

data Token = Token { string  :: String
                   , line :: Int
                   , column :: Int
                   , fileName :: String
                   } deriving (Show)
