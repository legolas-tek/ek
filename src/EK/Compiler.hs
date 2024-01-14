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
import Data.List (elemIndex, isPrefixOf, find)
import Data.Functor ((<&>))
import Control.Monad.State.Lazy

data Env = Env
  { args :: [(String, Bool)]
  , capturable :: [(String, Bool)]
  , captured :: [String]
  , result :: Result
  , fnName :: String
  , lambdaCount :: Int
  }

type Result = Map String Insts

showBytecode :: Result -> String
showBytecode = concatMap showEntry . toList
    where showEntry (key, value) = key ++ ":\n" ++ unlines (map (("\t" ++) . show) value)

compileToVM :: [TypedStmt] -> Either String Result
compileToVM stmts = Right $ compileStmts stmts

compileStmts :: [TypedStmt] -> Result
compileStmts = foldr (union . compileStmt) empty

patternToArgument :: (Int, TFuncPatternItem) -> [String]
patternToArgument (_, ArgPattern _ name _) = [name]
patternToArgument (_, SymbolPattern _) = []
patternToArgument (i, PlaceholderPattern) = ["_" ++ show i]

patternArguments :: TFuncPattern -> [String]
patternArguments (FuncPattern items _ _) = concatMap patternToArgument (zip [0..] items)

compileStmt :: TypedStmt -> Result
compileStmt (FuncDef pattern expr) = result $ execState (compileFn expr) (Env (zip (patternArguments pattern) (patternLazinesses pattern)) [] [] empty (show $ patternToName pattern) 0)
compileStmt (AtomDef name) = fromList [(name, [Push $ AtomValue name, Ret])]
compileStmt (StructDef _ items) = fromList $ zipWith fieldAccessor [0..] items
  where fieldAccessor i (StructElem name _) = ("_ " ++ name, [LoadArg 0, Extract i, Ret])
compileStmt _ = empty

compileFn :: TExpr -> State Env ()
compileFn expr = do
  args' <- gets args
  case args' of
    [] -> createFn expr -- first arg is void, but we never load_arg so it's ok
    [_] -> createFn expr -- easy case
    (x:xs) -> do
      outsideEnv <- get
      let lambdaName = fnName outsideEnv ++ "\\" ++ fst x
      put Env { args = xs
              , capturable = x : capturable outsideEnv
              , captured = []
              , result = result outsideEnv
              , fnName = lambdaName
              , lambdaCount = lambdaCount outsideEnv
              }
      compileFn expr -- recursive call
      insideEnv <- get
      put outsideEnv { args = [x]
                     , result = result insideEnv
                     , lambdaCount = lambdaCount insideEnv
                     }
      captures <- mapM compileCapture (reverse $ captured insideEnv)
      modify $ \env -> env { result = result env <> fromList [(fnName env, captures ++ [GetEnv lambdaName, Closure (length $ captured insideEnv), Ret])] }

compileExpr :: TExpr -> State Env Insts
compileExpr (IntegerLit i) = return [Push (IntegerValue i)]
compileExpr (FloatLit i) = return [Push (FloatValue i)]
compileExpr (StringLit s) = return [Push (StringValue s)]
compileExpr (EK.Ast.Call name callItems) = do
  call <- compileCall (show name)
  items <- compileCallItems callItems
  let needsCallVoid = isFn call && null items
  return $ call ++ items ++ (if needsCallVoid then [Push $ AtomValue "void", VirtualMachine.Call] else [])
  where isFn [GetEnv _] = True
        isFn _ = False
compileExpr (Lambda name expr) = do
  outsideEnv <- get
  let lambdaName = fnName outsideEnv ++ "\\" ++ name ++ show (lambdaCount outsideEnv)
  put Env { args = [(name, False)]
          , capturable = filter (not . isPrefixOf "_" . fst) $ args outsideEnv ++ capturable outsideEnv
          , captured = []
          , result = result outsideEnv
          , fnName = lambdaName
          , lambdaCount = lambdaCount outsideEnv + 1
          }
  createFn expr
  insideEnv <- get
  put outsideEnv { result = result insideEnv, lambdaCount = lambdaCount insideEnv }
  captures <- mapM compileCapture (reverse $ captured insideEnv)
  return $ captures ++ [GetEnv lambdaName, Closure (length captures)]
compileExpr (StructLit name items) = do
  items' <- concat <$> mapM compileExpr items
  return $ items' ++ [Construct (show name) (length items)]
compileExpr (TypeCheck expr typ) = do
  expr' <- compileExpr expr
  return $ expr' ++ [CheckConvertible typ]

createFn :: TExpr -> State Env ()
createFn expr = do
  args' <- gets args
  capturable' <- gets capturable
  content <- compileExpr expr
  additional <- concat <$> mapM handleArg (reverse capturable' ++ args')
  env <- get
  put env { result = result env <> fromList [(fnName env, content ++ additional ++ [Ret])] }
    where handleArg x | "_" `isPrefixOf` fst x = (++[VirtualMachine.Call]) <$> compileCall (fst x)
                      | otherwise = return []

compileCall :: String -> State Env [Instruction]
compileCall name = do
  env <- get
  case elemIndex name (fst <$> args env) of
    Just i | snd (args env !! i) -> return $ lazyLoadArg i
           | otherwise -> return [LoadArg i]
    Nothing -> case find ((== name) . fst) (capturable env) of
      Just arg@(_, True) -> lazyLoadArg <$> capture arg
      Just arg@(_, False) -> (:[]) . LoadArg <$> capture arg
      Nothing -> return [GetEnv name]
  where lazyLoadArg i = [LoadArg i, Push $ AtomValue "void", VirtualMachine.Call]

compileCapture :: String -> State Env Instruction
compileCapture name = do
  env <- get
  case elemIndex name (fst <$> args env) of
    Just i -> return $ LoadArg i
    Nothing -> case find ((== name) . fst) (capturable env) of
      Just arg@(_, _) -> LoadArg <$> capture arg
      Nothing -> error "impossible: value is captured but not in scope"

capture :: (String, Bool) -> State Env Int
capture arg = do
  env <- get
  let ret = length $ args env
  put env { args = args env ++ [arg], captured = captured env ++ [fst arg] }
  return ret

compileCallItems :: [TExpr] -> State Env Insts
compileCallItems items = concat <$> mapM compileCallItem items

compileCallItem :: TExpr -> State Env Insts
compileCallItem expr = compileExpr expr <&> (++ [VirtualMachine.Call])
