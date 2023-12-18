{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- vm
-}

module VirtualMachine
    ( exec
    ) where

data VMValue = IntegerValue Integer
             | BooleanValue Bool
             deriving (Show, Eq)

data Operator = Add
              | Sub
              | Mul
              | Div
              deriving (Show, Eq)

data Instruction = Push VMValue
                 | CallOp Operator
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

applyOp :: Operator -> VMValue -> VMValue -> Either String VMValue
applyOp op (IntegerValue v1) (IntegerValue v2)
  = Right $ IntegerValue $ getOp op v1 v2
applyOp _ _ _ = Left "Invalid operands for operator"

getOp :: Operator -> (Integer -> Integer -> Integer)
getOp Add = (+)
getOp Sub = (-)
getOp Mul = (*)
getOp Div = div
