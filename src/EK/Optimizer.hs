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

import qualified Data.Map as Map (lookup, filterWithKey, foldrWithKey, mapWithKey, keys)
import qualified Data.Set as Set
import Data.List (nub)

optimizeBytecode :: Result -> Result
optimizeBytecode = fmap optimizeInsts . deleteNotUsedFunc .  deleteSameInstsOfFunc . deleteNotUsedFunc

optimizeInsts :: Insts -> Insts
optimizeInsts [] = []
optimizeInsts (Closure 0: rest) = optimizeInsts rest
optimizeInsts (Push (IntegerValue x) : Push (IntegerValue y) : CallOp Add : rest) = Push (IntegerValue (x + y)) : optimizeInsts rest
optimizeInsts (Push (IntegerValue x) : Push (IntegerValue y) : CallOp Sub : rest) = Push (IntegerValue (x - y)) : optimizeInsts rest
optimizeInsts (Push (IntegerValue x) : Push (IntegerValue y) : CallOp Mul : rest) = Push (IntegerValue (x * y)) : optimizeInsts rest
optimizeInsts (inst : rest) = inst : optimizeInsts rest

deleteNotUsedFunc :: Result -> Result
deleteNotUsedFunc insts = deleteNotUsedFunc' insts (getCalledFunctionsFromMain insts)

deleteNotUsedFunc' :: Result -> [String] -> Result
deleteNotUsedFunc' insts functionsUsed = Map.filterWithKey (\k _ -> k `elem` functionsUsed) insts

getCalledFunctionsFromMain :: Result -> [String]
getCalledFunctionsFromMain insts = "main" : getCalledFunctions insts "main"

getCalledFunctions :: Result -> String -> [String]
getCalledFunctions insts func = visitCalledFunctions insts Set.empty [func]

visitCalledFunctions :: Result -> Set.Set String -> [String] -> [String]
visitCalledFunctions _ _ [] = []
visitCalledFunctions insts visited (x : xs)
  | x `Set.member` visited = visitCalledFunctions insts visited xs
  | otherwise =
      case Map.lookup x insts of
        Nothing -> visitCalledFunctions insts visited xs
        Just insts' -> let calledFuncs = getUsedFunctions insts'
                       in nub $ calledFuncs ++ visitCalledFunctions insts (Set.insert x visited) (xs ++ calledFuncs)

getUsedFunctions :: Insts -> [String]
getUsedFunctions [] = []
getUsedFunctions (GetEnv x : xs) = x : getUsedFunctions xs
getUsedFunctions (_ : xs) = getUsedFunctions xs

deleteSameInstsOfFunc :: Result -> Result
deleteSameInstsOfFunc insts = deleteSameInstsOfFunc' insts (Map.keys insts)

deleteSameInstsOfFunc' :: Result -> [String] -> Result
deleteSameInstsOfFunc' insts [] = insts
deleteSameInstsOfFunc' insts (x : xs) =
  case Map.lookup x insts of
    Nothing -> deleteSameInstsOfFunc' insts xs
    Just insts' ->
      let sameInsts = detectSameInsts x insts' insts
      in if null sameInsts
         then deleteSameInstsOfFunc' insts xs
         else deleteSameInstsOfFunc' (changeFuncNameInInsts x sameInsts insts) xs

detectSameInsts :: String -> Insts -> Result -> [String]
detectSameInsts fname insts = Map.foldrWithKey (\k v acc -> if k /= fname && v == insts then k : acc else acc) []

updateFuncName :: String -> [String] -> Insts -> Insts
updateFuncName _ _ [] = []
updateFuncName name namesToChange (GetEnv x : xs) = GetEnv (if x `elem` namesToChange then name else x) : updateFuncName name namesToChange xs
updateFuncName name namesToChange (x : xs) = x : updateFuncName name namesToChange xs

changeFuncNameInInsts :: String -> [String] -> Result -> Result
changeFuncNameInInsts name namesToChange = Map.mapWithKey (\_ v -> updateFuncName name namesToChange v)
