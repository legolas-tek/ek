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

import qualified Data.Map as Map (lookup, filterWithKey)
import qualified Data.Set as Set
import Data.List (nub)

getUsedFunctions :: Insts -> [String]
getUsedFunctions [] = []
getUsedFunctions (GetEnv x : xs) = x : getUsedFunctions xs
getUsedFunctions (_ : xs) = getUsedFunctions xs

visitCalledFunctions :: Result -> Set.Set String -> [String] -> [String]
visitCalledFunctions _ _ [] = []
visitCalledFunctions insts visited (x : xs)
  | x `Set.member` visited = visitCalledFunctions insts visited xs
  | otherwise =
      case Map.lookup x insts of
        Nothing -> visitCalledFunctions insts visited xs
        Just insts' -> let calledFuncs = getUsedFunctions insts'
                       in nub $ calledFuncs ++ visitCalledFunctions insts (Set.insert x visited) (xs ++ calledFuncs)

getCalledFunctions :: Result -> String -> [String]
getCalledFunctions insts func = visitCalledFunctions insts Set.empty [func]

getCalledFunctionsFromMain :: Result -> [String]
getCalledFunctionsFromMain insts = "main" : getCalledFunctions insts "main"

deleteNotUsedFunc :: Result -> Result
deleteNotUsedFunc insts = deleteNotUsedFunc' insts (getCalledFunctionsFromMain insts)

deleteNotUsedFunc' :: Result -> [String] -> Result
deleteNotUsedFunc' insts functionUsed = Map.filterWithKey (\k _ -> k `elem` functionUsed) insts

optimizeBytecode :: Result -> Result
optimizeBytecode = fmap optimizeInsts . deleteNotUsedFunc

optimizeInsts :: Insts -> Insts
optimizeInsts [] = []
optimizeInsts (Closure 0: rest) = optimizeInsts rest
optimizeInsts (Call : Ret : rest) = TailCall : optimizeInsts rest
optimizeInsts (Push (IntegerValue x) : Push (IntegerValue y) : CallOp Add : rest) = Push (IntegerValue (x + y)) : optimizeInsts rest
optimizeInsts (Push (IntegerValue x) : Push (IntegerValue y) : CallOp Sub : rest) = Push (IntegerValue (x - y)) : optimizeInsts rest
optimizeInsts (Push (IntegerValue x) : Push (IntegerValue y) : CallOp Mul : rest) = Push (IntegerValue (x * y)) : optimizeInsts rest
optimizeInsts (inst : rest) = inst : optimizeInsts rest
