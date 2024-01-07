{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Compiler
--}

module EK.Compiler
  ( compileToVM
  , Result
  , showBytecode
  ) where

import VirtualMachine hiding (Env)
import EK.Ast
import Data.Map (Map, fromList, empty, union, toList)
import Data.List (elemIndex, isPrefixOf)
import Data.Functor ((<&>))
import Control.Monad.State.Lazy

data Env = Env
  { args :: [String]
  , capturable :: [String]
  , captured :: [String]
  , result :: Result
  , fnName :: String
  }

type Result = Map String Insts

showBytecode :: Result -> String
showBytecode = concatMap showEntry . toList
    where showEntry (key, value) = key ++ ":\n" ++ unlines (map (("\t" ++) . show) value)

compileToVM :: [TotalStmt] -> Either String Result
compileToVM stmts = Right $ compileStmts stmts

compileStmts :: [TotalStmt] -> Result
compileStmts = foldr (union . compileStmt) empty

patternToArgument :: (Int, FuncPatternItem) -> [String]
patternToArgument (_, ArgPattern _ name _) = [name]
patternToArgument (_, SymbolPattern _) = []
patternToArgument (i, PlaceholderPattern) = ["_" ++ show i]

patternArguments :: FuncPattern -> [String]
patternArguments (FuncPattern' items _ _) = concatMap patternToArgument (zip [0..] items)

compileStmt :: TotalStmt -> Result
compileStmt (FuncDef pattern expr) = result $ execState (compileFn expr) (Env (patternArguments pattern) [] [] empty (show $ patternToName pattern))
compileStmt _ = empty

compileFn :: Expr -> State Env ()
compileFn expr = do
  args' <- gets args
  case args' of
    [] -> createFn expr -- first arg is void, but we never load_arg so it's ok
    [_] -> createFn expr -- easy case
    (x:xs) -> do
      outsideEnv <- get
      let lambdaName = fnName outsideEnv ++ "\\" ++ x
      put Env { args = xs
              , capturable = x : capturable outsideEnv
              , captured = []
              , result = result outsideEnv
              , fnName = lambdaName
              }
      compileFn expr -- recursive call
      insideEnv <- get
      put outsideEnv { args = [x]
                     , result = result insideEnv
                     }
      captures <- mapM compileCall $ captured insideEnv
      modify $ \env -> env { result = result env <> fromList [(fnName env, captures ++ [GetEnv lambdaName, Closure (length captures), Ret])] }

compileExpr :: Expr -> State Env Insts
compileExpr (IntegerLit i) = return [Push (IntegerValue i)]
compileExpr (StringLit s) = return [Push (StringValue s)]
compileExpr (EK.Ast.Call name callItems) = do
  call <- compileCall (show name)
  items <- compileCallItems callItems
  let needsCallVoid = isFn call && null items
  return $ (call:items) ++ (if needsCallVoid then [Push $ AtomValue "void", VirtualMachine.Call] else [])
  where isFn (GetEnv _) = True
        isFn _ = False
compileExpr (Lambda name expr) = do
  outsideEnv <- get
  let lambdaName = fnName outsideEnv ++ "\\" ++ name
  put Env { args = [name]
          , capturable = filter (not . isPrefixOf "_") $ args outsideEnv ++ capturable outsideEnv
          , captured = []
          , result = result outsideEnv
          , fnName = lambdaName
          }
  createFn expr
  insideEnv <- get
  put outsideEnv { result = result insideEnv }
  captures <- mapM compileCall $ captured insideEnv
  return $ captures ++ [GetEnv lambdaName, Closure (length captures)]

createFn :: Expr -> State Env ()
createFn expr = do
  args' <- gets args
  capturable' <- gets capturable
  content <- compileExpr expr
  additional <- concat <$> mapM handleArg (reverse capturable' ++ args')
  env <- get
  put env { result = result env <> fromList [(fnName env, content ++ additional ++ [Ret])] }
    where handleArg x | "_" `isPrefixOf` x = (:[VirtualMachine.Call]) <$> compileCall x
                      | otherwise = return []

compileCall :: String -> State Env Instruction
compileCall name = do
  env <- get
  case elemIndex name (args env) of
    Just i -> return $ LoadArg i
    Nothing -> (if name `elem` capturable env then (do
      i <- capture name
      return $ LoadArg i) else return $ GetEnv name)

capture :: String -> State Env Int
capture name = do
  env <- get
  let ret = length $ args env
  put env { args = args env ++ [name], captured = captured env ++ [name] }
  return ret

compileCallItems :: [Expr] -> State Env Insts
compileCallItems items = concat <$> mapM compileCallItem items

compileCallItem :: Expr -> State Env Insts
compileCallItem expr = compileExpr expr <&> (++ [VirtualMachine.Call])
