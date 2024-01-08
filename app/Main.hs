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
import Token
import Tokenizer
import EK.Ast
import EK.Parser
import EK.ExprParser
import EK.Builtins
import EK.Compiler
import Diagnostic

import Control.Exception (try, catch, IOException)
import Control.Monad (when)
import Data.Bifunctor (first)

parseLine :: String -> Either String ([Ast], String)
parseLine line = first show (runParser (many parseSExpr) line) >>=
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

parseStmtOrExpr :: [TotalStmt] -> [Token] -> IO (Either [TotalStmt] Expr)
parseStmtOrExpr partials tokens = do
    res <- try (parseDocument tokens) :: IO (Either IOException ([TotalStmt], [Diagnostic]))
    case res of
            Right result -> return (Left $ fst result)
            Left _ -> Right <$> (either (fail . show) return $ parseReplExpr partials tokens)


mainLoop :: [TotalStmt] -> String -> IO ()
mainLoop env rest = do
    printPrompt $ null rest
    eofVal <- isEOF
    when eofVal exitSuccess
    line <- getLine
    (tokens, diags) <- either (fail . show) return $ tokenizer "stdin" line
    let res = parseStmtOrExpr env tokens
    toEither <- try res :: IO (Either IOException (Either [TotalStmt] Expr))
    case toEither of
        Left err -> do
            mapM_ (putStrLn . show) (diags)
            print err
            mainLoop env rest
        Right (Left stmts) -> do
            mainLoop (env ++ stmts) rest
        Right (Right expr) -> do
            insts <- either fail return $ compileToVM (FuncDef (FuncPattern [SymbolPattern "main"] Nothing Nothing) expr : env)
            runVM insts `catch` (\e -> putStrLn $ show (e :: IOException))
            mainLoop env rest

main :: IO ()
main = mainLoop [] ""
