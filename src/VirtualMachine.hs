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

import Data.Map (Map, lookup)

data VMValue = IntegerValue Integer
             | BooleanValue Bool
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
                 | PushEnv String
                 deriving (Show, Eq)

type Stack = [VMValue]
type Insts = [Instruction]
type Env = Map String VMValue

exec :: Env -> Insts -> Stack -> Either String Stack
exec _ [] stack = Right stack
exec _ (Ret:_) stack = Right stack
exec env (Push v:insts) stack = exec env insts (v:stack)
exec env (Call:insts) (OperatorValue op:v1:v2:stack) = applyOp op v1 v2
  >>= \result -> exec env insts (result:stack)
exec env (Call:insts) (FunctionValue fn:stack) = exec env fn stack
  >>= \stack' -> exec env insts stack'
exec env (Call:insts) (StringValue s:stack) = exec env insts (StringValue s:stack)
exec _ (Call:_) (OperatorValue _:_) = Left "Not enough arguments for operator"
exec _ (Call:_) _ = Left "Cannot call value of non-function type"
exec env (JmpFalse offset:insts) (BooleanValue False:stack)
  = exec env (drop offset insts) stack
exec env (JmpFalse _:insts) (BooleanValue True:stack) = exec env insts stack
exec _ (JmpFalse _:_) _ = Left "Invalid condition"
exec env (Dup:insts) (v:stack) = exec env insts (v:v:stack)
exec _ (Dup:_) [] = Left "No value to duplicate"
exec env (PushEnv value : insts) stack =
  case sValue of
    Just val -> exec env insts (val : stack)
    Nothing  -> Left "Couldn't find requested VMValue in env"
  where
    sValue = Data.Map.lookup value env

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
applyOp Eq a b = Right $ BooleanValue $ a == b
applyOp Less (IntegerValue a) (IntegerValue b)
  = Right $ BooleanValue $ a < b
applyOp _ _ _ = Left "Invalid operands for operator"
