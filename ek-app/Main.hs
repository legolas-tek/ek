{--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Main
--}

module Main (main) where

import Serialize
import EK.Builtins

import System.Environment (getArgs)
import Data.Maybe (listToMaybe)

import Control.Monad (void)

main :: IO ()
main = getArgs >>= maybe (putStrLn "Usage: ./ek <file.eko>") handleArg . listToMaybe

handleArg :: String -> IO ()
handleArg arg = loadResult arg >>= either print (void . runVM)
