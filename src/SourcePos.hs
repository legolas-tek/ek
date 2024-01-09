{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- SourcePos
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module SourcePos
    ( SourcePos(..),
      Parsable(..)
    ) where

import Data.Word

data SourcePos = SourcePos { sourceName :: String
                           , sourceLine :: Int
                           , sourceColumn :: Int
                           } deriving (Eq)
class Parsable a where
    advance :: SourcePos -> a -> SourcePos

instance Parsable String where
  advance pos@SourcePos { .. } _ = pos { sourceColumn = sourceColumn + 1 }

instance Parsable Char where
    advance (SourcePos name line _) '\n' = SourcePos name (line + 1) 1
    advance (SourcePos name line column) _ = SourcePos name line (column + 1)

instance Parsable Word8 where
    advance (SourcePos name line column) _ = SourcePos name line (column + 1)

instance Show SourcePos where
    show (SourcePos name line column) = name ++ ":" ++ show line ++ ":" ++ show column
