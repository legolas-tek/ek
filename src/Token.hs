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

import SourcePos

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
               | Arrow
               deriving (Show, Eq)

data Token = Token { lexeme  :: String
                   , sourcePos :: SourcePos
                   , tokenType :: TokenType
                   } deriving (Show, Eq)

instance Parsable Token where
    advance _ = sourcePos
