{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Main
--}

{-# LANGUAGE TupleSections #-}

module Main (main) where

import System.IO
import System.Exit

import Parser
import Lisp
import Ast
import Evaluation

import Control.Monad (when)

parseLine :: String -> Either String ([Ast], String)
parseLine line = runParser (many parseSExpr) line >>=
    \(sexprs, rest) -> (, if null rest then "" else rest ++ "\n") <$> mapM sexprToAST sexprs

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
printResult _ (Right (env, val, rest)) = print val >> return (env, rest)

printPrompt :: Bool -> IO ()
printPrompt new = hIsTerminalDevice stdin >>= \isTerm ->
    when isTerm $ putStr (if new then "glados> " else "  ... > ") >> hFlush stdout

mainLoop :: Environment -> String -> IO ()
mainLoop env rest = do
    printPrompt $ null rest
    eof <- isEOF
    when eof $ exitWith (ExitSuccess)
    line <- getLine
    (env', rest') <- printResult env (handleResult (rest ++ line) env)
    mainLoop env' rest'

main :: IO ()
main = mainLoop defaultEnv ""
