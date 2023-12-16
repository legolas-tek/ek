{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Main
--}

module Main (main) where

import Parser
import Lisp
import Ast

parseLine :: String -> Either String ([Ast], String)
parseLine line = runParser (many parseSExpr) line >>=
    \(sexprs, rest) -> (\asts -> (asts, rest)) <$> (mapM sexprToAST sexprs)

handleResult :: Either String ([Ast], String) -> IO ()
handleResult (Left err) = putStrLn err
handleResult (Right (asts, _)) = mapM_ print asts

main :: IO ()
main = do
    line <- getLine
    handleResult $ parseLine line
    main
