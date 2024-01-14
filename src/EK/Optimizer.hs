{--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Optimizer
--}

{-# LANGUAGE LambdaCase #-}

module EK.Optimizer
  ( optimizeBytecode,
    optimizeInsts,
    inlineInsts,
    inlineResult,
    deleteNotUsedFunc,
    deleteSameInstsOfFunc,
    convertInlinedInsts,
  )
where

import Data.List (nub)
import qualified Data.Map as Map (filterWithKey, keys, lookup, map, mapWithKey)
import qualified Data.Set as Set
import EK.Compiler
import VirtualMachine

optimizeBytecode :: Result -> Result
optimizeBytecode = fmap optimizeInsts . deleteNotUsedFunc . inlineResult . deleteSameInstsOfFunc . deleteNotUsedFunc

optimizeInsts :: Insts -> Insts
optimizeInsts [] = []
optimizeInsts (Closure 0 : rest) = optimizeInsts rest
optimizeInsts (Call : Ret : rest) = TailCall : optimizeInsts rest
optimizeInsts (Push x : Push y : CallOp op : rest) = case applyOp op x y of
  Right v -> Push v : optimizeInsts rest
  Left _ -> Push x : Push y : CallOp op : optimizeInsts rest
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
        Just insts' ->
          let calledFuncs = getUsedFunctions insts'
           in nub $ calledFuncs ++ visitCalledFunctions insts (Set.insert x visited) (xs ++ calledFuncs)

getUsedFunctions :: Insts -> [String]
getUsedFunctions [] = []
getUsedFunctions (GetEnv x : xs) = x : getUsedFunctions xs
getUsedFunctions (_ : xs) = getUsedFunctions xs

deleteSameInstsOfFunc :: Result -> Result
deleteSameInstsOfFunc insts = foldr deleteIfSame insts (Map.keys insts)

deleteIfSame :: String -> Result -> Result
deleteIfSame x insts = maybe insts (deleteIfSame' x insts) (Map.lookup x insts)

deleteIfSame' :: String -> Result -> Insts -> Result
deleteIfSame' x insts insts' =
  let sameInsts = detectSameInsts x insts' insts
   in if null sameInsts then insts else changeFuncNameInInsts x sameInsts insts

detectSameInsts :: String -> Insts -> Result -> [String]
detectSameInsts fname insts = Map.keys . Map.filterWithKey (\k v -> k /= fname && k /= "main" && v == insts)

updateFuncName :: String -> [String] -> Insts -> Insts
updateFuncName _ _ [] = []
updateFuncName name namesToChange (GetEnv x : xs) = GetEnv (if x `elem` namesToChange then name else x) : updateFuncName name namesToChange xs
updateFuncName name namesToChange (x : xs) = x : updateFuncName name namesToChange xs

changeFuncNameInInsts :: String -> [String] -> Result -> Result
changeFuncNameInInsts name namesToChange = Map.map (updateFuncName name namesToChange)

convertInlinedInsts :: Insts -> VMValue -> Insts
convertInlinedInsts insts value = concatMap (convertInst value) insts

convertInst :: VMValue -> Instruction -> Insts
convertInst value (LoadArg 0) = [Push value]
convertInst _ (Ret) = []
convertInst _ inst = [inst]

inlineResult :: Result -> Result
inlineResult res = Map.mapWithKey (\fn insts -> inlineInsts insts res fn) res

inlineInsts :: Insts -> Result -> String -> Insts
inlineInsts [] _ _ = []
inlineInsts (GetEnv fn : Push value : Call : xs) env currFn
  | fn == currFn = [GetEnv fn, Push value, Call] ++ inlineInsts xs env currFn
  | otherwise =
      case Map.lookup fn env of
        Nothing -> [GetEnv fn, Push value, Call] ++ inlineInsts xs env currFn
        Just insts -> inlineInsts (convertInlinedInsts insts value) env currFn ++ inlineInsts xs env currFn
inlineInsts (x : xs) env currFn = x : inlineInsts xs env currFn
