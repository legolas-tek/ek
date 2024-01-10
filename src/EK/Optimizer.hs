{--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Optimizer
--}

module EK.Optimizer
  ( optimizeBytecode
  , optimizeInsts
  ) where

import VirtualMachine
import EK.Compiler

optimizeBytecode :: Result -> Result
optimizeBytecode = fmap optimizeInsts

optimizeInsts :: Insts -> Insts
optimizeInsts [] = []
optimizeInsts (Closure 0: rest) = optimizeInsts rest
optimizeInsts (Push (IntegerValue x) : Push (IntegerValue y) : CallOp Add : rest) = Push (IntegerValue (x + y)) : optimizeInsts rest
optimizeInsts (Push (IntegerValue x) : Push (IntegerValue y) : CallOp Sub : rest) = Push (IntegerValue (x - y)) : optimizeInsts rest
optimizeInsts (Push (IntegerValue x) : Push (IntegerValue y) : CallOp Mul : rest) = Push (IntegerValue (x * y)) : optimizeInsts rest
optimizeInsts (inst : rest) = inst : optimizeInsts rest
