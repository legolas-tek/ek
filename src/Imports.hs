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
import Data.Maybe (fromMaybe)


setDefaultImportsPath :: IO ()
setDefaultImportsPath = do
    curEnv <- fromMaybe "" <$> lookupEnv "EK_LIBRARY_PATH"
    getExecutablePath >>= (\curDir -> setEnv "EK_LIBRARY_PATH" (curEnv ++ ":./:" ++ curDir ++ "/../lib/ek/:")) . takeDirectory
