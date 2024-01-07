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
  [ ("builtin print", [GetEnv "builtin print\\impl", Ret])
  , ("builtin print\\impl", [LoadArg 0, CallOp Print, void, Ret])
  , ("builtin exit", [GetEnv "builtin exit\\impl", Ret])
  , ("builtin exit\\impl", [LoadArg 0, CallOp Exit, void, Ret])
  , ("builtin add", [GetEnv "builtin add\\a", Ret])
  , ("builtin add\\a", [LoadArg 0, GetEnv "builtin add\\b", Closure 1, Ret])
  , ("builtin add\\b", [LoadArg 0, LoadArg 1, CallOp Add, Ret])
  , ("builtin sub", [GetEnv "builtin sub\\a", Ret])
  , ("builtin sub\\a", [LoadArg 0, GetEnv "builtin sub\\b", Closure 1, Ret])
  , ("builtin sub\\b", [LoadArg 0, LoadArg 1, CallOp Sub, Ret])
  , ("builtin mul", [GetEnv "builtin mul\\a", Ret])
  , ("builtin mul\\a", [LoadArg 0, GetEnv "builtin mul\\b", Closure 1, Ret])
  , ("builtin mul\\b", [LoadArg 0, LoadArg 1, CallOp Mul, Ret])
  , ("builtin div", [GetEnv "builtin div\\a", Ret])
  , ("builtin div\\a", [LoadArg 0, GetEnv "builtin div\\b", Closure 1, Ret])
  , ("builtin div\\b", [LoadArg 0, LoadArg 1, CallOp Div, Ret])
  ]
