module SExpr
  ( SExpr (Integer, Symbol, List),
    getSymbol,
    printTree
  ) where

data SExpr = Integer Int
           | Symbol String
           | List [SExpr]
          deriving Show

type Error = String

getSymbol :: SExpr -> Maybe String
getSymbol (Symbol s) = Just s
getSymbol _ = Nothing

printTree :: SExpr -> Either Error String
printTree (Symbol s) = Right ("a Symbol " ++ s)
printTree(Integer i) = Right ("a Number " ++ show i)
printTree(List l) = mapM printTree l >>= \e -> Right ("a List with " ++ unwords e)

