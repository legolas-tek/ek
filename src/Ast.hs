module Ast
    ( Ast
    ) where

import SExpr

data Ast = Define {
    name :: String,
    value :: Ast
}
          | IntegerLit Int
          | AstSymbol String
          deriving Show


sexprToAST :: SExpr -> Maybe Ast
sexprToAST (Symbol s) = Just (AstSymbol s)
sexprToAST (Integer i) = Just (IntegerLit i)
sexprToAST (List [Symbol "define", Symbol s, v]) = sexprToAST v >>= \e -> Just (Define {name = s, value = e})
sexprToAST _ = Nothing
