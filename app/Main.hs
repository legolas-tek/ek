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
