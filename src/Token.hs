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
               | LazyKw
               | PrecedenceKw
               | ImportKw
               | IsKw
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
               | FloatLiter
               | StringLiter
               | TextIdentifier
               | OperatorIdentifier
               | Arrow
               | Backslash
               deriving (Show, Eq)

data Token = Token { lexeme  :: String
                   , sourcePos :: SourcePos
                   , tokenType :: TokenType
                   } deriving (Show, Eq)

instance Parsable Token where
    advance _ = sourcePos
