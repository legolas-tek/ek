{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Imports
-}


module Imports
    ( setDefaultImportsPath
    ) where

import System.Environment (setEnv, lookupEnv, getExecutablePath)
import System.FilePath (takeDirectory)


setDefaultImportsPath :: IO ()
setDefaultImportsPath = do
    curEnv <- lookupEnv "EK_LIBRARY_PATH"
    case curEnv of
        Nothing -> getExecutablePath >>= (\curDir -> setEnv "EK_LIBRARY_PATH" ("./:" ++ curDir ++ "/../lib/ek/:")) . takeDirectory
        Just current -> getExecutablePath >>= (\curDir -> setEnv "EK_LIBRARY_PATH" (current ++ ":./:" ++ curDir ++ "/../lib/ek/:")) . takeDirectory

