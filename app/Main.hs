module Main (main) where

import Parser
import Lisp
import Ast

parseLine :: String -> Either String Ast
parseLine line = do
    (sexpr, _) <- runParser parseSExpr line
    ast <- sexprToAST sexpr
    return ast

main :: IO ()
main = do
    line <- getLine
    case parseLine line of
        Left err -> putStrLn err
        Right ast -> print ast
    main

