{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- eval
-}

{-# LANGUAGE TupleSections #-}

module Evaluation
  ( RuntimeValue(..)
  , EvalResult
  , EvalError
  , Environment
  , evalAst
  , defaultEnv
  , evalBody
  ) where

import Data.Bits (xor)

import Ast

type EvalError = String

type Environment = [(String, RuntimeValue)]

data BuiltinFn = BuiltinFn (Environment -> [RuntimeValue] -> Either EvalError RuntimeValue)

data RuntimeValue = IntegerValue Integer
                  | BooleanValue Bool
                  | FunctionValue BuiltinFn
                  | VoidValue
                  | StringValue String
                  deriving (Eq)

type EvalResult = Either EvalError (Environment, RuntimeValue)

envLookup :: Environment -> String -> Either EvalError RuntimeValue
envLookup [] name = Left $ "Symbol " ++ name ++ " not found"
envLookup ((name', val):env) name
  | name == name' = Right val
  | otherwise = envLookup env name

evalBody :: Environment -> [Ast] -> EvalResult
evalBody env [] = Right (env, VoidValue)
evalBody env (x:[]) = evalAst env x
evalBody env (x:xs) = evalAst env x >>= \(nextEnv, _) -> evalBody nextEnv xs

evalLambda :: Environment -> [String] -> [Ast] -> Environment -> [RuntimeValue] -> Either EvalError RuntimeValue
evalLambda closedEnv names body currentEnv args
  | length names /= length args = Left $ "Function expected " ++ show (length names) ++ " arguments but got " ++ show (length args) ++ " arguments"
  | otherwise = snd <$> evalBody ((zip names args) ++ closedEnv ++ currentEnv) body

evalIf :: Environment -> RuntimeValue -> Ast -> Ast -> EvalResult
evalIf env (BooleanValue True) trueCase _ = evalAst env trueCase
evalIf env (BooleanValue False) _ falseCase = evalAst env falseCase
evalIf _ _ _ _ = Left "Invalid condition in if"

evalAst :: Environment -> Ast -> EvalResult
evalAst env (Define name val) = evalAst env val >>= \(_, value) -> Right ((name, value):env, VoidValue)
evalAst env (If cond trueCase falseCase) = evalAst env cond >>= \(_, condValue) -> evalIf env condValue trueCase falseCase
evalAst env (IntegerLit i) = Right (env, IntegerValue i)
evalAst env (Symbol s) = (env,) <$> envLookup env s
evalAst env (StringLit s) = Right (env, StringValue s)
evalAst env (Lambda names body) =
  Right (env, FunctionValue $ BuiltinFn $ evalLambda env names body)
evalAst env (Call fn args) = do
  fn' <- snd <$> evalAst env fn
  args' <- mapM (evalAst env) args
  call env fn' (snd <$> args')

call :: Environment -> RuntimeValue -> [RuntimeValue] -> EvalResult
call env (FunctionValue (BuiltinFn fn)) args = (env,) <$> fn env args
call _ _ _ = Left $ "Cannot call value of non-function type"

defaultEnv :: Environment
defaultEnv = [ ("#t", BooleanValue True)
             , ("#f", BooleanValue False)
             , ("+", arithmeticFn sum)
             , ("*", arithmeticFn product)
             , ("-", arithmeticFn difference)
             , ("/", arithmeticFn quotient)
             , ("max", arithmeticFn maximum)
             , ("min", arithmeticFn minimum)
             , ("not", FunctionValue $ BuiltinFn $ const $ mapBoolean not)
             , ("and", FunctionValue $ BuiltinFn $ const $ mapBooleans and)
             , ("or", FunctionValue $ BuiltinFn $ const $ mapBooleans or)
             , ("xor", FunctionValue $ BuiltinFn $ const $ mapTwoBooleans xor)
             , ("eq?", boolOperator (==))
             , ("<",  compareOperator (<))
             , (">",  compareOperator (>))
             , ("<=", compareOperator (<=))
             , (">=", compareOperator (>=))
             , ("=", boolOperator (==))
             ]

instance Eq BuiltinFn where
  _ == _ = False

instance Show RuntimeValue where
  show (IntegerValue v) = show v
  show (StringValue v) = show v
  show (BooleanValue True) = "#t"
  show (BooleanValue False) = "#f"
  show (FunctionValue _) = "(lambda)"
  show VoidValue = ""

mapIntegers :: [RuntimeValue] -> Either EvalError [Integer]
mapIntegers = mapM mapInteger
  where mapInteger (IntegerValue v) = Right v
        mapInteger _ = Left "Expected an integer"

arithmeticFn :: ([Integer] -> Integer) -> RuntimeValue
arithmeticFn fn = FunctionValue $ BuiltinFn $ const $ ((IntegerValue . fn) <$>) . mapIntegers

difference :: Num a => [a] -> a
difference = foldr (-) 0

quotient :: Integral a => [a] -> a
quotient = foldr div 1

mapBoolean :: (Bool -> Bool) -> [RuntimeValue] -> Either EvalError RuntimeValue
mapBoolean fn [BooleanValue v] = Right $ BooleanValue $ fn v
mapBoolean _ _ = Left "Expected a boolean"

mapTwoBooleans :: (Bool -> Bool -> Bool) -> [RuntimeValue] -> Either EvalError RuntimeValue
mapTwoBooleans fn [BooleanValue v1, BooleanValue v2] = Right $ BooleanValue $ fn v1 v2
mapTwoBooleans _ _ = Left "Expected two booleans"

mapBooleans :: ([Bool] -> Bool) -> [RuntimeValue] -> Either EvalError RuntimeValue
mapBooleans fn values = BooleanValue . fn <$> mapM mapBoolean' values
  where mapBoolean' (BooleanValue v) = Right v
        mapBoolean' _ = Left "Expected a boolean"

compareOperator :: (Integer -> Integer -> Bool) -> RuntimeValue
compareOperator fn = FunctionValue $ BuiltinFn $ const $ compareOperatorFn fn

compareOperatorFn :: (Integer -> Integer -> Bool) -> [RuntimeValue] -> Either EvalError RuntimeValue
compareOperatorFn fn [IntegerValue a, IntegerValue b] = Right $ BooleanValue $ fn a b
compareOperatorFn _ _ = Left "Expected two integers"

boolOperator :: (RuntimeValue -> RuntimeValue -> Bool) -> RuntimeValue
boolOperator fn = FunctionValue $ BuiltinFn $ const $ boolOperatorFn fn

boolOperatorFn :: (RuntimeValue -> RuntimeValue -> Bool) -> [RuntimeValue] -> Either EvalError RuntimeValue
boolOperatorFn fn [a,  b] = Right $ BooleanValue $ fn a b
boolOperatorFn _ _ = Left "Expected two values"
