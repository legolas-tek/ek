--
-- EPITECH PROJECT, 2023
-- B-FUN-500-PAR-5-2-glados-jeremy.elalouf
-- File description:
-- Token
--

module Token
    ( Token(..)
    , TokenType(..)
    ) where

data TokenType = AtomKw
               | StructKw
               | TypeKw
               | FnKw
               | ExternKw
               | Equal
               | CurlyOpen
               | CurlyClose
               | Comma
               | UnderScore
               | ParenOpen
               | ParenClose
               | Colon
               | ColonColon
               | Pipe
               | BracketOpen
               | BracketClose
               | DotDot
               | IntLiter
               | StringLiter
               | TextIdentifier
               | OperatorIdentifier
               deriving (Show)

data Token = Token { string  :: String
                   , line :: Int
                   , column :: Int
                   , fileName :: String
                   , tokenType :: TokenType
                   } deriving (Show)
