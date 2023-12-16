module Main (main) where

import Parser
import SExpr
import Lisp
import Ast

main :: IO ()
main = do
    line <- getLine
    let sexprResult = runParser parseSExpr line
    case sexprResult of
        Left err -> putStrLn ("Erreur de parsing : " ++ show err)
        Right (expr, _) -> do
            let astResult = sexprToAST expr
            case astResult of
                Left err -> putStrLn ("Erreur de conversion en AST : " ++ show err)
                Right ast -> putStrLn ("AST : " ++ show ast)
    main

