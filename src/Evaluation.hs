module Evaluation
  (
    RuntimeValue(..)
  ) where

import Ast

type EvalError = String

type Environment = [(String, RuntimeValue)]

data RuntimeValue = IntegerValue Int
                  | BooleanValue Bool
                  | BuiltInFunction ([RuntimeValue] -> RuntimeValue)

evalAst :: Environment -> Ast -> (Environment, Either EvalError RuntimeValue)
evalAst env (Define name (IntegerLit val)) = ((name, IntegerValue val):env, Right $ IntegerValue 1)

