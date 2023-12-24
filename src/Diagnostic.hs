--
-- EPITECH PROJECT, 2023
-- B-FUN-500-PAR-5-2-glados-jeremy.elalouf
-- File description:
-- Diagnostic
--

module Diagnostic
    ( Diagnostic(..)
    , Severity(..)
    ) where

import Token

data Severity = Info
              | Warning
              | Error
              deriving (Show, Eq)

data Diagnostic = Diagnostic {
    severity :: Severity,
    message  :: String,
    token    :: [Token]
} deriving (Show)
