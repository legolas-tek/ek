--
-- EPITECH PROJECT, 2023
-- B-FUN-500-PAR-5-2-glados-jeremy.elalouf
-- File description:
-- Diagnostic
--

{-# LANGUAGE RecordWildCards #-}

module Diagnostic
    ( Diagnostic(..)
    , Severity(..)
    ) where

import SourcePos

data Severity = Info
              | Warning
              | Error
              deriving (Show, Eq)

data Diagnostic = Diagnostic {
    severity :: Severity,
    message  :: String,
    sourceLocation :: SourcePos
}

instance Show Diagnostic where
    show (Diagnostic {..}) =
        show severity ++ ": " ++ message ++ " at " ++ show sourceLocation
