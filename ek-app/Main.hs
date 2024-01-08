{--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Main
--}

module Main (main) where

import Serialize
import EK.Builtins (runVM)
import System.Environment (getArgs)

main :: IO ()
main = do
    (arg:_) <- getArgs
    loadResult arg >>= \load -> case load of
        Left err -> putStrLn (show err)
        Right res -> runVM res
