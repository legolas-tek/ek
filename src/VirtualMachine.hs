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
import System.Exit (exitWith, ExitCode(..), exitSuccess)
import System.IO (hPutStr, stderr)

data VMValue = IntegerValue Integer
             | AtomValue String
             | FunctionValue Insts
             | ClosureValue Insts [VMValue]
             | StringValue String
             deriving (Eq)

instance Show VMValue where
  show (IntegerValue v) = show v
  show (AtomValue v) = v
  show (FunctionValue _) = "(function)"
  show (ClosureValue _ _) = "(function)"
  show (StringValue v) = v

data Operator = Add
              | Sub
              | Mul
              | Div
              | Eq
              | Less
              | Print
              | EPrint
              | Exit
              | ReadLine
              | ToString
              deriving (Eq)

instance Show Operator where
  show Add = "add"
  show Sub = "sub"
  show Mul = "mul"
  show Div = "div"
  show Eq = "eq"
  show Less = "less"
  show Print = "print"
  show Exit = "exit"
  show EPrint = "eprint"
  show ReadLine = "readLine"
  show ToString = "toString"

data Instruction = Push VMValue
                 | Call
                 | CallOp Operator
                 | JmpFalse Int
                 | Dup
                 | Ret
                 | LoadArg Int
                 | GetEnv String
                 | Closure Int
                 deriving (Eq)

type Args = [VMValue]
type Insts = [Instruction]
type Stack = [VMValue]
type Env = Map String VMValue

-- instance Eq Instruction where
--   Push a == Push b = a == b
--   Call == Call = True
--   CallOp a == CallOp b = a == b
--   JmpFalse a == JmpFalse b = a == b
--   Dup == Dup = True
--   Ret == Ret = True
--   LoadArg a == LoadArg b = a == b
--   GetEnv a == GetEnv b = a == b
--   Closure a == Closure b = a == b
--   _ == _ = False

instance Show Instruction where
  show (Push (IntegerValue v)) = "push " ++ show v
  show (Push (AtomValue v)) = "push " ++ v
  show (Push (FunctionValue _)) = "push function" -- impossible
  show (Push (ClosureValue _ _)) = "push function" -- impossible
  show (Push (StringValue v)) = "push " ++ show v
  show Call = "call"
  show (CallOp op) = "call_op " ++ show op
  show (JmpFalse offset) = "jmp_false " ++ show offset
  show Dup = "dup"
  show Ret = "ret"
  show (LoadArg offset) = "load_arg " ++ show offset
  show (GetEnv value) = "getenv " ++ value
  show (Closure count) = "closure " ++ show count

exec :: Env -> Args -> Insts -> Stack -> IO VMValue
exec _ _ [] (s:_) = return s
exec _ _ [] [] = fail "No value on stack"
exec _ _ (Ret:_) (s:_) = return s
exec _ _ (Ret:_) [] = fail "No value on stack"
exec env args (Push v:insts) stack = exec env args insts (v:stack)
exec env args (CallOp Print:insts) (v:stack) = print v >> exec env args insts stack
exec env args (CallOp EPrint:insts) (v:stack) = hPutStr stderr (show v) >> exec env args insts stack
exec env args (CallOp ReadLine:insts) stack = getLine >>= \line -> exec env args insts (StringValue line:stack)
exec env args (CallOp ToString:insts) (v:stack) = exec env args insts (StringValue (show v):stack)
exec _ _ (CallOp Exit:_) ((IntegerValue 0):_) = exitSuccess
exec _ _ (CallOp Exit:_) ((IntegerValue v):_) = exitWith $ ExitFailure $ fromIntegral v
exec env args (CallOp op:insts) (v1:v2:stack) =
  either fail (\val -> exec env args insts (val:stack)) (applyOp op v1 v2)
exec _ _ (CallOp _:_) _ = fail "Not enough arguments for operator"
exec env args (Call:insts) (arg:FunctionValue fn:stack) = exec env [arg] fn []
  >>= \result -> exec env args insts (result:stack)
exec env args (Call:insts) (arg:ClosureValue fn captures:stack) = exec env (arg:captures) fn []
  >>= \result -> exec env args insts (result:stack)
exec _ _ (Call:_) (v:f:_) = error $ "Cannot call value of non-function type: " ++ show f ++ " " ++ show v
exec _ _ (Call:_) _ = fail "Not enough values for call"
exec env args (JmpFalse offset:insts) (AtomValue "false":stack)
  = exec env args (drop offset insts) stack
exec env args (JmpFalse _:insts) (AtomValue "true":stack) = exec env args insts stack
exec _ _ (JmpFalse _:_) (value:_) = fail $ "Invalid condition: " ++ show value
exec _ _ (JmpFalse _:_) [] = fail "Invalid condition: no value on stack"
exec env args (Dup:insts) (v:stack) = exec env args insts (v:v:stack)
exec _ _ (Dup:_) [] = fail "No value to duplicate"
exec env args (LoadArg offset:insts) stack = exec env args insts (args !! offset:stack)
exec env args (GetEnv value:insts) stack = case Data.Map.lookup value env of
  Just val -> exec env args insts (val:stack)
  Nothing  -> fail $ "Could not find `" ++ value ++ "' in environment"
exec env args (Closure count:insts) (FunctionValue fn:stack) = exec env args insts (ClosureValue fn (take count stack):drop count stack)
exec _ _ (Closure _:_) _ = fail "Cannot create closure of non-function type"

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
