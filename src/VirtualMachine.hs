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
             | AtomValue String
             | FunctionValue Insts
             | StringValue String
             deriving (Eq)

instance Show VMValue where
  show (IntegerValue v) = show v
  show (AtomValue v) = v
  show (FunctionValue insts) = concatMap show insts
  show (StringValue v) = v

data Operator = Add
              | Sub
              | Mul
              | Div
              | Eq
              | Less
              | Print
              deriving (Eq)

instance Show Operator where
  show Add = "add"
  show Sub = "sub"
  show Mul = "mul"
  show Div = "div"
  show Eq = "eq"
  show Less = "less"
  show Print = "print"

data Instruction = Push VMValue
                 | Call
                 | CallOp Operator
                 | JmpFalse Int
                 | Dup
                 | Ret
                 | LoadArg Int
                 | GetEnv String
                 deriving (Eq)

type Args = [VMValue]
type Insts = [Instruction]
type Stack = [VMValue]
type Env = Map String VMValue

instance Show Instruction where
  show (Push (IntegerValue v)) = "push " ++ show v
  show (Push (AtomValue v)) = "push " ++ v
  show (Push (FunctionValue _)) = "push function"
  show (Push (StringValue v)) = "push " ++ show v
  show Call = "call"
  show (CallOp op) = "call_op " ++ show op
  show (JmpFalse offset) = "jmp_false " ++ show offset
  show Dup = "dup"
  show Ret = "ret"
  show (LoadArg offset) = "load_arg " ++ show offset
  show (GetEnv value) = "getenv " ++ value

exec :: Env -> Args -> Insts -> Stack -> IO (Either String VMValue)
exec _ _ [] (s:_) = return $ Right s
exec _ _ [] [] = return $ Left "No value on stack"
exec _ _ (Ret:_) (s:_) = return $ Right s
exec _ _ (Ret:_) [] = return $ Left "No value on stack"
exec env args (Push v:insts) stack = exec env args insts (v:stack)
exec env args (CallOp Print:insts) (v:stack) = putStrLn (show v) >> exec env args insts stack
exec env args (CallOp op:insts) (v1:v2:stack) = do
  result <- applyOp op v1 v2
  case result of
    Right val -> exec env args insts (val:stack)
    Left err -> return $ Left err
exec _ _ (CallOp _:_) _ = return $ Left "Not enough arguments for operator"
exec env args (Call:insts) (arg:FunctionValue fn:stack) = do
  result <- exec env [arg] fn []
  case result of
    Right val -> exec env args insts (val:stack)
    Left err -> return $ Left err
exec _ _ (Call:_) _ = return $ Left "Cannot call value of non-function type"
exec env args (JmpFalse offset:insts) (AtomValue "false":stack)
  = exec env args (drop offset insts) stack
exec env args (JmpFalse _:insts) (AtomValue "true":stack) = exec env args insts stack
exec _ _ (JmpFalse _:_) _ = return $ Left "Invalid condition"
exec env args (Dup:insts) (v:stack) = exec env args insts (v:v:stack)
exec _ _ (Dup:_) [] = return $ Left "No value to duplicate"
exec env args (LoadArg offset:insts) stack = exec env args insts (args !! offset:stack)
exec env args (GetEnv value:insts) stack = case Data.Map.lookup value env of
  Just val -> exec env args insts (val:stack)
  Nothing  -> return $ Left "No value in env"

applyOp :: Operator -> VMValue -> VMValue -> IO (Either String VMValue)
applyOp Add (IntegerValue a) (IntegerValue b)
  = return $ Right $ IntegerValue $ a + b
applyOp Sub (IntegerValue a) (IntegerValue b)
  = return $ Right $ IntegerValue $ a - b
applyOp Mul (IntegerValue a) (IntegerValue b)
  = return $ Right $ IntegerValue $ a * b
applyOp Div (IntegerValue _) (IntegerValue 0)
  = return $ Left "Division by zero"
applyOp Div (IntegerValue a) (IntegerValue b)
  = return $ Right $ IntegerValue $ a `div` b
applyOp Eq a b = return $ Right $ AtomValue (if a == b then "true" else "false")
applyOp Less (IntegerValue a) (IntegerValue b)
  = return $ Right $ AtomValue (if a < b then "true" else "false")
applyOp _ _ _ = return $ Left "Invalid operands for operator"
