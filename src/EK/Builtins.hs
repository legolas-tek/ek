{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- builtins
-}

module EK.Builtins
  ( builtins
  , runVM
  ) where

import VirtualMachine
import EK.Compiler

import qualified Data.Map as Map

void :: Instruction
void = Push $ AtomValue "void"

runVM :: Result -> IO VMValue
runVM res = do
  mainFn <- maybe (fail "No main function") return $ Map.lookup "main" res
  let insts = res <> builtins
  let env = FunctionValue <$> insts
  exec env [] mainFn []

builtins :: Result
builtins = Map.fromList
  [ ("builtin print", [GetEnv "builtin print\\impl", Ret])
  , ("builtin print\\impl", [LoadArg 0, CallOp Print, void, Ret])
  , ("builtin eprint", [GetEnv "builtin eprint\\impl", Ret])
  , ("builtin eprint\\impl", [LoadArg 0, CallOp EPrint, void, Ret])
  , ("builtin readLine", [CallOp ReadLine, Ret])
  , ("builtin concat", [GetEnv "builtin concat\\a", Ret])
  , ("builtin concat\\a", [LoadArg 0, GetEnv "builtin concat\\b", Closure 1, Ret])
  , ("builtin concat\\b", [LoadArg 0, LoadArg 1, CallOp Concat, Ret])
  , ("builtin charAt", [GetEnv "builtin charAt\\a", Ret])
  , ("builtin charAt\\a", [LoadArg 0, GetEnv "builtin charAt\\b", Closure 1, Ret])
  , ("builtin charAt\\b", [LoadArg 0, LoadArg 1, CallOp CharAt, Ret])
  , ("builtin toString", [GetEnv "builtin toString\\impl", Ret])
  , ("builtin toString\\impl", [LoadArg 0, CallOp ToString, Ret])
  , ("builtin length", [GetEnv "builtin length\\impl", Ret])
  , ("builtin length\\impl", [LoadArg 0, CallOp Length, Ret])
  , ("builtin toInt", [GetEnv "builtin toInt\\impl", Ret])
  , ("builtin toInt\\impl", [LoadArg 0, CallOp ToInt, Ret])
  , ("builtin toFloat", [GetEnv "builtin toFloat\\impl", Ret])
  , ("builtin toFloat\\impl", [LoadArg 0, CallOp ToFloat, Ret])
  , ("builtin toChar", [GetEnv "builtin toChar\\impl", Ret])
  , ("builtin toChar\\impl", [LoadArg 0, CallOp ToChar, Ret])
  , ("builtin toCodePoint", [GetEnv "builtin toCodePoint\\impl", Ret])
  , ("builtin toCodePoint\\impl", [LoadArg 0, CallOp ToCodePoint, Ret])
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
  , ("builtin eq", [GetEnv "builtin eq\\a", Ret])
  , ("builtin eq\\a", [LoadArg 0, GetEnv "builtin eq\\b", Closure 1, Ret])
  , ("builtin eq\\b", [LoadArg 0, LoadArg 1, CallOp Eq, Ret])
  , ("builtin lt", [GetEnv "builtin lt\\a", Ret])
  , ("builtin lt\\a", [LoadArg 0, GetEnv "builtin lt\\b", Closure 1, Ret])
  , ("builtin lt\\b", [LoadArg 0, LoadArg 1, CallOp Less, Ret])
  , ("builtin if", [GetEnv "builtin if\\cond", Ret])
  , ("builtin if\\cond", [LoadArg 0, GetEnv "builtin if\\then", Closure 1, Ret])
  , ("builtin if\\then", [LoadArg 0, LoadArg 1, GetEnv "builtin if\\else", Closure 2, Ret])
  , ("builtin if\\else", [LoadArg 1, JmpFalse 4, LoadArg 2, void, Call, Ret, LoadArg 0, void, Call, Ret])
  ]
