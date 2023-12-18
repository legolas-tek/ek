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
             deriving (Show, Eq)

data Operator = Add
              | Sub
              | Mul
              | Div
              | Eq
              | Less
              deriving (Show, Eq)

data Instruction = Push VMValue
                 | CallOp Operator
                 | JmpFalse Int
                 | Ret
                 deriving (Show, Eq)

type Stack = [VMValue]
type Insts = [Instruction]

exec :: Insts -> Stack -> Either String Stack
exec [] stack = Right stack
exec (Ret:_) stack = Right stack
exec (Push v:insts) stack = exec insts (v:stack)
exec (CallOp op:insts) (v1:v2:stack) = applyOp op v1 v2
  >>= \result -> exec insts (result:stack)
exec (CallOp _:_) _ = Left "Not enough arguments for operator"
exec (JmpFalse offset:insts) (BooleanValue False:stack)
  = exec (drop offset insts) stack
exec (JmpFalse _:insts) (BooleanValue True:stack) = exec insts stack
exec (JmpFalse _:_) _ = Left "Invalid condition"

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
