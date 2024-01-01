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
import SourcePos(SourcePos(..), Parsable(..))

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
                   , line :: Int
                   , column :: Int
                   , fileName :: String
                   , tokenType :: TokenType
                   } deriving (Show)

instance Parsable Token where
    advance _ token = SourcePos {sourceLine = line token, sourceColumn = column token, sourceName = fileName token}
