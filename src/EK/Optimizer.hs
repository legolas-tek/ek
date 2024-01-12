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

import qualified Data.Map as Map (lookup, filterWithKey, foldrWithKey, keys, filter)
import qualified Data.Maybe as Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.List (nub)
import Debug.Trace
import EK.Compiler (Result)

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

detectSameInsts :: String -> Insts -> Result -> [String]
detectSameInsts fname insts = Map.foldrWithKey (\k v acc -> if k /= fname && v == insts then k : acc else acc) []

changeFuncNameInInsts :: String -> String -> Insts -> Insts
changeFuncNameInInsts _ _ [] = []
changeFuncNameInInsts oldName newName (GetEnv x : xs) = GetEnv (if x == oldName then newName else x) : changeFuncNameInInsts oldName newName xs
changeFuncNameInInsts oldName newName (x :xs) = x : changeFuncNameInInsts oldName newName xs

-- replaceFuncName :: String -> [String] -> Result -> Result
-- replaceFuncName _ [] insts = insts
-- replaceFuncName newName (oldName : xs) insts =

deleteNotUsedFunc' :: Result -> [String] -> Result
deleteNotUsedFunc' insts functionUsed = Map.filterWithKey (\k _ -> k `elem` functionUsed) insts

optimizeBytecode :: Result -> Result
optimizeBytecode res =
  let printInsts = Maybe.fromMaybe [] (Map.lookup "print _" res)
  in trace (show $ detectSameInsts "print _" printInsts res) (fmap optimizeInsts (deleteNotUsedFunc res))

optimizeInsts :: Insts -> Insts
optimizeInsts [] = []
optimizeInsts (Closure 0: rest) = optimizeInsts rest
optimizeInsts (Push (IntegerValue x) : Push (IntegerValue y) : CallOp Add : rest) = Push (IntegerValue (x + y)) : optimizeInsts rest
optimizeInsts (Push (IntegerValue x) : Push (IntegerValue y) : CallOp Sub : rest) = Push (IntegerValue (x - y)) : optimizeInsts rest
optimizeInsts (Push (IntegerValue x) : Push (IntegerValue y) : CallOp Mul : rest) = Push (IntegerValue (x * y)) : optimizeInsts rest
optimizeInsts (inst : rest) = inst : optimizeInsts rest
