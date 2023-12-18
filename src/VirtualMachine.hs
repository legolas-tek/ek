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
    ) where

data VMValue = IntegerValue Integer
             | BooleanValue Bool
             | OperatorValue Operator
             | FunctionValue Insts
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
                 deriving (Show, Eq)

type Stack = [VMValue]
type Insts = [Instruction]

exec :: Insts -> Stack -> Either String Stack
exec [] stack = Right stack
exec (Ret:_) stack = Right stack
exec (Push v:insts) stack = exec insts (v:stack)
exec (Call:insts) (OperatorValue op:v1:v2:stack) = applyOp op v1 v2
  >>= \result -> exec insts (result:stack)
exec (Call:insts) (FunctionValue fn:stack) = exec fn stack
  >>= \stack' -> exec insts stack'
exec (Call:_) (OperatorValue _:_) = Left "Not enough arguments for operator"
exec (Call:_) _ = Left "Cannot call value of non-function type"
exec (JmpFalse offset:insts) (BooleanValue False:stack)
  = exec (drop offset insts) stack
exec (JmpFalse _:insts) (BooleanValue True:stack) = exec insts stack
exec (JmpFalse _:_) _ = Left "Invalid condition"
exec (Dup:insts) (v:stack) = exec insts (v:v:stack)
exec (Dup:_) [] = Left "No value to duplicate"

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
