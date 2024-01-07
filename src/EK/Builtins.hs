{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- builtins
-}

module EK.Builtins
  ( builtins
  ) where

import VirtualMachine
import EK.Compiler

import qualified Data.Map as Map

void :: Instruction
void = Push $ AtomValue "void"

builtins :: Result
builtins = Map.fromList
  [
    ("builtin print", [GetEnv "builtin print\\impl", Ret]),
    ("builtin print\\impl", [LoadArg 0, CallOp Print, void, Ret]),
    ("builtin exit", [GetEnv "builtin exit\\impl", Ret]),
    ("builtin exit\\impl", [LoadArg 0, CallOp Exit, void, Ret])
  ]
