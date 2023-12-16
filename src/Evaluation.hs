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
  , evalAst
  , defaultEnv
  , evalBody
  ) where

import Ast

type EvalError = String

type Environment = [(String, RuntimeValue)]

data BuiltinFn = BuiltinFn (Environment -> [RuntimeValue] -> Either EvalError RuntimeValue)

data RuntimeValue = IntegerValue Integer
                  | BooleanValue Bool
                  | FunctionValue BuiltinFn
                  | VoidValue
                  deriving (Eq, Show)

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

evalAst :: Environment -> Ast -> EvalResult
evalAst env (Define name val) = evalAst env val >>= \(_, value) -> Right ((name, value):env, VoidValue)
evalAst env (IntegerLit i) = Right (env, IntegerValue i)
evalAst env (Symbol s) = (env,) <$> envLookup env s
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
             ]

instance Eq BuiltinFn where
  _ == _ = False

instance Show BuiltinFn where
  show _ = "Function"
