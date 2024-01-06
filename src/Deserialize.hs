{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
--
--}

module Deserialize () where

import qualified Data.ByteString as B
import qualified Data.Map as Map
import EK.Compiler(Result)
import Serialize

isCorrectFormat :: B.ByteString -> Bool
isCorrectFormat content | B.head content == 42 = True
                        | otherwise = False

deserializeResult :: String -> B.ByteString -> Result
deserializeResult path bytecode
    | isCorrectFormat bytecode = bytecodeToResult bytecode Map.empty
    | otherwise = Map.empty

bytecodeToResult :: B.ByteString -> Result -> Result
bytecodeToResult bs map | B.null bs = map
                        | otherwise = bytecodeToResult bs map
