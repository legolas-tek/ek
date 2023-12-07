module Ast
    ( Ast(..)
    ) where

import SExpr

data Ast = Define String Ast
          | IntegerLit Int
          | AstSymbol String
          | Lambda [String] [Ast]
          | Call Ast [Ast]
          deriving Show

extractString :: SExpr -> Maybe String
extractString (Symbol s) = Just s
extractString _ = Nothing

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (Symbol s) = Just (AstSymbol s)
sexprToAST (Integer i) = Just (IntegerLit i)
sexprToAST (List [Symbol "define", Symbol s, v]) = sexprToAST v >>= \e -> Just (Define s e)
-- using do notation
sexprToAST (List [Symbol "lambda", List args, List expr]) = do
  argsList <- mapM extractString args
  body <- mapM sexprToAST expr
  return (Lambda argsList body)
sexprToAST (List (Symbol x:xs)) = mapM sexprToAST xs >>= \e -> Just (Call (AstSymbol x) e)
sexprToAST _ = Nothing
