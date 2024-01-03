{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- vm
-}

module VirtualMachine
    ( exec
    , VMValue(..)
    , Operator(..)
    , Instruction(..)
    , Stack
    , Insts
    , Env
    ) where

import Data.Map (Map, lookup, delete)

data VMValue = IntegerValue Integer
             | AtomValue String
             | OperatorValue Operator
             | FunctionValue Insts
             | StringValue String
             deriving (Show, Eq)

data Operator = Add
              | Sub
              | Mul
              | Div
              | Eq
              | Less
              deriving (Show, Eq)

data Instruction = Push VMValue
                 | Call
                 | JmpFalse Int
                 | Dup
                 | Ret
                 | LoadArg Int
                 | PopEnv String
                 deriving (Show, Eq)

type Args = [VMValue]
type Insts = [Instruction]
type Stack = [VMValue]
type Env = Map String VMValue

exec :: Env -> Args -> Insts -> Stack -> Either String VMValue
exec _ _ [] (s:_) = Right s
exec _ _ [] [] = Left "No value on stack"
exec _ _ (Ret:_) (s:_) = Right s
exec _ _ (Ret:_) [] = Left "No value on stack"
exec env args (Push v:insts) stack = exec env args insts (v:stack)
exec env args (Call:insts) (OperatorValue op:v1:v2:stack) = applyOp op v1 v2
  >>= \result -> exec env args insts (result:stack)
exec env args (Call:insts) (FunctionValue fn:arg:stack) = exec env [arg] fn []
  >>= \result -> exec env args insts (result:stack)
exec _ _ (Call:_) (OperatorValue _:_) = Left "Not enough arguments for operator"
exec _ _ (Call:_) _ = Left "Cannot call value of non-function type"
exec env args (JmpFalse offset:insts) (AtomValue "false":stack)
  = exec env args (drop offset insts) stack
exec env args (JmpFalse _:insts) (AtomValue "true":stack) = exec env args insts stack
exec _ _ (JmpFalse _:_) _ = Left "Invalid condition"
exec env args (Dup:insts) (v:stack) = exec env args insts (v:v:stack)
exec _ _ (Dup:_) [] = Left "No value to duplicate"
exec env args (PopEnv value : insts) stack = popEnv value env
  >>= \(val, env') -> exec env' args insts (val : stack)
exec env args (LoadArg offset:insts) stack = exec env args insts (stack !! (length stack - offset - 1):stack)

applyOp :: Operator -> VMValue -> VMValue -> Either String VMValue
applyOp Add (IntegerValue a) (IntegerValue b)
  = Right $ IntegerValue $ a + b
applyOp Sub (IntegerValue a) (IntegerValue b)
  = Right $ IntegerValue $ a - b
applyOp Mul (IntegerValue a) (IntegerValue b)
  = Right $ IntegerValue $ a * b
applyOp Div (IntegerValue _) (IntegerValue 0)
  = Left "Division by zero"
applyOp Div (IntegerValue a) (IntegerValue b)
  = Right $ IntegerValue $ a `div` b
applyOp Eq a b = Right $ AtomValue (if a == b then "true" else "false")
applyOp Less (IntegerValue a) (IntegerValue b)
  = Right $ AtomValue (if a < b then "true" else "false")
applyOp _ _ _ = Left "Invalid operands for operator"

popEnv :: String -> Env -> Either String (VMValue, Env)
popEnv value env =
  case Data.Map.lookup value env of
    Just val -> Right (val, Data.Map.delete value env)
    Nothing  -> Left "No value to pop from env"
