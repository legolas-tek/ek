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
    , applyOp
    ) where

import Data.Map (Map, lookup)
import System.Exit (exitWith, ExitCode(..), exitSuccess)
import System.IO (hPutStr, stderr)
import Data.List (intercalate)
import EK.Types
import Text.Read (readMaybe)

data VMValue = IntegerValue Integer
             | FloatValue Double
             | AtomValue String
             | FunctionValue Insts
             | ClosureValue Insts [VMValue]
             | StringValue String
             | StructValue String [VMValue]
             deriving (Eq)

instance Show VMValue where
  show (IntegerValue v) = show v
  show (FloatValue v) = show v
  show (AtomValue v) = v
  show (FunctionValue _) = "(function)"
  show (ClosureValue _ _) = "(function)"
  show (StringValue v) = v
  show (StructValue name vs) = name ++ "{" ++ intercalate ", " (map show vs) ++ "}"

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
              | Concat
              | ToInt
              | ToFloat
              | CharAt
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
  show Concat = "concat"
  show ToInt = "toInt"
  show ToFloat = "toFloat"
  show CharAt = "charAt"

data Instruction = Push VMValue
                 | Call
                 | TailCall
                 | CallOp Operator
                 | JmpFalse Int
                 | Dup
                 | Ret
                 | LoadArg Int
                 | GetEnv String
                 | Closure Int
                 | Construct String Int
                 | Extract Int
                 | CheckConvertible Type
                 deriving (Eq)

type Args = [VMValue]
type Insts = [Instruction]
type Stack = [VMValue]
type Env = Map String VMValue

instance Show Instruction where
  show (Push (StringValue v)) = "push " ++ show v
  show (Push v) = "push " ++ show v
  show Call = "call"
  show TailCall = "tail_call"
  show (CallOp op) = "call_op " ++ show op
  show (JmpFalse offset) = "jmp_false " ++ show offset
  show Dup = "dup"
  show Ret = "ret"
  show (LoadArg offset) = "load_arg " ++ show offset
  show (GetEnv value) = "getenv " ++ value
  show (Closure count) = "closure " ++ show count
  show (Construct name count) = "construct " ++ name ++ " " ++ show count
  show (Extract offset) = "extract " ++ show offset
  show (CheckConvertible t) = "check_convertible " ++ show t

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
exec env args (CallOp ToInt:insts) (v:stack) = case v of
  IntegerValue _ -> exec env args insts (v:stack)
  FloatValue f -> exec env args insts (IntegerValue (floor f):stack)
  StringValue s -> case readMaybe s of
    Just i -> exec env args insts (IntegerValue i:stack)
    Nothing -> fail $ "Cannot convert string " ++ s ++ " to int"
  _ -> fail $ "Cannot convert value of type " ++ show v ++ " to int"
exec env args (CallOp ToFloat:insts) (v:stack) = case v of
  IntegerValue i -> exec env args insts (FloatValue (fromIntegral i):stack)
  FloatValue _ -> exec env args insts (v:stack)
  StringValue s -> case readMaybe s of
    Just f -> exec env args insts (FloatValue f:stack)
    Nothing -> fail $ "Cannot convert string " ++ s ++ " to float"
  _ -> fail $ "Cannot convert value of type " ++ show v ++ " to float"
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
exec env _ (TailCall:_) (arg:FunctionValue fn:_) = exec env [arg] fn []
exec env _ (TailCall:_) (arg:ClosureValue fn captures:_) = exec env (arg:captures) fn []
exec _ _ (TailCall:_) (v:f:_) = error $ "Cannot tail_call value of non-function type: " ++ show f ++ " " ++ show v
exec _ _ (TailCall:_) _ = fail "Not enough values for tail_call"
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
exec env args (Construct name count:insts) stack = exec env args insts (StructValue name (reverse $ take count stack):drop count stack)
exec env args (Extract offset:insts) (StructValue _ vs:stack) = exec env args insts (vs !! offset:stack)
exec _ _ (Extract _:_) _ = fail "Cannot extract field from non-struct type"
exec env args (CheckConvertible t:insts) (v:stack) = exec env args insts (atomicBool (convertible (runtimeType v) t):stack)
exec _ _ (CheckConvertible _:_) [] = fail "No value to check convertible"

runtimeType :: VMValue -> Type
runtimeType (IntegerValue i) = intTy i
runtimeType (FloatValue _) = structTy "float"
runtimeType (StringValue _) = structTy "string"
runtimeType (AtomValue a) = atomTy a
runtimeType (FunctionValue _) = functionTy AnyTy AnyTy
runtimeType (ClosureValue _ _) = functionTy AnyTy AnyTy
runtimeType (StructValue name _) = structTy name

atomicBool :: Bool -> VMValue
atomicBool True = AtomValue "true"
atomicBool False = AtomValue "false"

-- int int
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
applyOp Eq a b = Right $ atomicBool $ a == b
applyOp Less (IntegerValue a) (IntegerValue b)
  = Right $ atomicBool $ a < b
applyOp Concat (IntegerValue a) (IntegerValue b)
  = Right $ IntegerValue $ read $ show a ++ show b

-- float float
applyOp Add (FloatValue a) (FloatValue b)
  = Right $ FloatValue $ a + b
applyOp Sub (FloatValue a) (FloatValue b)
  = Right $ FloatValue $ a - b
applyOp Mul (FloatValue a) (FloatValue b)
  = Right $ FloatValue $ a * b
applyOp Div (FloatValue _) (FloatValue 0)
  = Left "Division by zero"
applyOp Div (FloatValue a) (FloatValue b)
  = Right $ FloatValue $ a / b
applyOp Less (FloatValue a) (FloatValue b)
  = Right $ atomicBool $ a < b
applyOp Concat (FloatValue a) (FloatValue b)
  = Right $ FloatValue $ read $ show a ++ show b

-- float int
applyOp Add (FloatValue a) (IntegerValue b)
  = Right $ FloatValue $ a + fromIntegral b
applyOp Sub (FloatValue a) (IntegerValue b)
  = Right $ FloatValue $ a - fromIntegral b
applyOp Mul (FloatValue a) (IntegerValue b)
  = Right $ FloatValue $ a * fromIntegral b
applyOp Div (FloatValue _) (IntegerValue 0)
  = Left "Division by zero"
applyOp Div (FloatValue a) (IntegerValue b)
  = Right $ FloatValue $ a / fromIntegral b
applyOp Less (FloatValue a) (IntegerValue b)
  = Right $ atomicBool $ a < fromIntegral b
applyOp Concat (FloatValue a) (IntegerValue b)
  = Right $ FloatValue $ read $ show a ++ show b

-- int float
applyOp Add (IntegerValue a) (FloatValue b)
  = Right $ FloatValue $ fromIntegral a + b
applyOp Sub (IntegerValue a) (FloatValue b)
  = Right $ FloatValue $ fromIntegral a - b
applyOp Mul (IntegerValue a) (FloatValue b)
  = Right $ FloatValue $ fromIntegral a * b
applyOp Div (IntegerValue _) (FloatValue 0)
  = Left "Division by zero"
applyOp Div (IntegerValue a) (FloatValue b)
  = Right $ FloatValue $ fromIntegral a / b
applyOp Less (IntegerValue a) (FloatValue b)
  = Right $ atomicBool $ fromIntegral a < b
applyOp Concat (IntegerValue a) (FloatValue b)
  = Right $ FloatValue $ read $ show a ++ show b

-- string
applyOp Concat (StringValue a) (StringValue b)
  = Right $ StringValue $ a ++ b
applyOp Add (StringValue a) (StringValue b)
  = Right $ StringValue $ a ++ b
applyOp Mul (StringValue a) (IntegerValue b)
  = Right $ StringValue $ concat $ replicate (fromIntegral b) a
applyOp Mul (IntegerValue a) (StringValue b)
  = Right $ StringValue $ concat $ replicate (fromIntegral a) b
applyOp Sub (StringValue a) (IntegerValue b)
  = Right $ StringValue $ take (length a - fromIntegral b) a
applyOp Sub (IntegerValue a) (StringValue b)
  = Right $ StringValue $ drop (fromIntegral a) b
applyOp CharAt (StringValue a) (IntegerValue b)
  = Right $ StringValue [a !! fromIntegral b]
applyOp CharAt (IntegerValue a) (StringValue b)
  = Right $ StringValue [b !! fromIntegral a]
applyOp _ _ _ = Left "Invalid operands for operator"
