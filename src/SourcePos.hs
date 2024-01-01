{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- SourcePos
-}

module SourcePos
    ( SourcePos(..),
      Parsable(..)
    ) where

data SourcePos = SourcePos { sourceName :: String
                           , sourceLine :: Int
                           , sourceColumn :: Int
                           }
class Parsable a where
    advance :: SourcePos -> a -> SourcePos

instance Parsable Char where
    advance (SourcePos name line _) '\n' = SourcePos name (line + 1) 1
    advance (SourcePos name line column) _ = SourcePos name line (column + 1)

instance Show SourcePos where
    show (SourcePos name line column) = name ++ ":" ++ show line ++ ":" ++ show column
