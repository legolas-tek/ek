{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Main
--}

{-# LANGUAGE TupleSections #-}

module Main (main) where

import System.IO
-- import System.Process

import Parser
import Lisp
import Ast
import Evaluation

parseLine :: String -> Either String ([Ast], String)
parseLine line = runParser (many parseSExpr) line >>=
    \(sexprs, rest) -> (, rest) <$> mapM sexprToAST sexprs

evalAsts :: Environment -> [Ast] -> EvalResult
evalAsts env [] = Right (env, VoidValue)
evalAsts env [x] = evalAst env x
evalAsts env (x:xs) = evalAst env x >>= \(env', _) -> evalAsts env' xs

handleResult :: String -> Environment -> Either EvalError (Environment, RuntimeValue, String)
handleResult s env = do
    (asts, rest) <- parseLine s
    (env', value) <- evalAsts env asts
    return (env', value, rest)
    
printResult :: Environment -> Either EvalError (Environment, RuntimeValue, String) -> IO (Environment, String)
printResult env (Left err) = putStrLn ("Error: " ++ err) >> return (env, "")
printResult _ (Right (env, VoidValue, rest)) = return (env, rest)
printResult _ (Right (env, val, rest)) = putStrLn (show val) >> return (env, rest)

printPrompt :: IO ()
printPrompt = hIsTerminalDevice stdin >>= \isTerm ->
    if isTerm then putStr "glados> " >> hFlush stdout else return ()

mainLoop :: Environment -> String -> IO ()
mainLoop env rest = do
    printPrompt
    line <- getLine
    let result = handleResult (rest ++ line) env
    (env', rest') <- printResult env result
    mainLoop env' rest'

main :: IO ()
main = mainLoop defaultEnv ""
