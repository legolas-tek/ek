{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- expression parser for ek
-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module EK.ExprParser
  ( parseExprs
  , parseReplExpr
  , parseExprsAdding
  ) where

import EK.Ast
import Token
import Parser
import EK.TokenParser
import Diagnostic

import Control.Monad (liftM2)
import Control.Applicative (Alternative(empty))
import Data.Monoid (Alt(..))
import Data.List (findIndex)

import EK.TypeParser

data FuncItem = FuncItem
  { funcName :: FunctionName
  , lazynesses :: [Bool]
  } deriving (Eq, Show)

lowestPrec :: Prec
lowestPrec = 0

primaryPrec :: Prec
primaryPrec = defaultPrec

parseExprs :: [PartialStmt] -> Either Diagnostic [TotalStmt]
parseExprs = parseExprsAdding []

parseExprsAdding :: [TotalStmt] -> [PartialStmt] -> Either Diagnostic [TotalStmt]
parseExprsAdding add partials = mapM (parseBody $ concatMap funcItems partials ++ concatMap funcItems add) partials

parseReplExpr :: [TotalStmt] -> [Token] -> Either Diagnostic Expr
parseReplExpr = parseExpr . concatMap funcItems

parseBody :: [FuncItem] -> PartialStmt -> Either Diagnostic TotalStmt
parseBody fi (FuncDef pat body) = FuncDef pat <$> parseExpr (args ++ fi) body
  where
    args = funcPatternItems pat >>= argFuncItems
    argFuncItems (ArgPattern _ s _) = [primaryFuncItem s]
    argFuncItems PlaceholderPattern = []
    argFuncItems (SymbolPattern _) = []
parseBody _ (ExternDef pat) = return $ ExternDef pat
parseBody _ (AtomDef name) = return $ AtomDef name
parseBody _ (TypeDef name ty) = return $ TypeDef name ty
parseBody _ (StructDef name elems) = return $ StructDef name elems
parseBody _ (ImportDef name) = return $ ImportDef name

primaryFuncItem :: String -> FuncItem
primaryFuncItem s = FuncItem (FunctionName [Symbol s] primaryPrec) []

parseExpr :: [FuncItem] -> [Token] -> Either Diagnostic Expr
parseExpr fi tokens = fst <$> runParser (parsePrec fi lowestPrec <* eof) tokens

funcItems :: Stmt a b -> [FuncItem]
funcItems (FuncDef pat _) = [patternToItem pat]
funcItems (ExternDef pat) = [patternToItem pat]
funcItems (AtomDef name) = [primaryFuncItem name]
funcItems (StructDef _ items) = map accessorItem items
  where accessorItem (StructElem name _) = FuncItem (FunctionName [Placeholder, Symbol name] primaryPrec) [False]
funcItems _ = []

patternToItem :: FuncPattern' a -> FuncItem
patternToItem pat = FuncItem (patternToName pat) (patternLazinesses pat)

parsePrec :: [FuncItem] -> Prec -> Parser Token Expr
parsePrec fi prec = primItem fi <|> parsePrefix fi prec >>= parseInfix fi prec

parsePrefix :: [FuncItem] -> Prec -> Parser Token CallItem
parsePrefix fi prec = getAlt (foldMap (Alt . parsePrefix' fi prec) fi) <|> fail "Could not resolve expression"

parsePrefix' :: [FuncItem] -> Prec -> FuncItem -> Parser Token CallItem
parsePrefix' fi prec fname@(FuncItem (FunctionName (Symbol s:ss) fnprec) _)
  = ExprCall <$> (identifierExact s >> createCall fname <$> parseFollowUp fi ss (max prec fnprec))
parsePrefix' _ _ _ = empty

parseInfix :: [FuncItem] -> Prec -> CallItem -> Parser Token Expr
parseInfix fi prec initial = typeCheck fi prec initial <|> getAlt (foldMap (Alt . parseInfix' fi prec initial) fi) <|> noInfix initial
  where noInfix (ExprCall i) = return i
        noInfix PlaceholderCall = fail "Invalid placeholder"

parseInfix' :: [FuncItem] -> Prec -> CallItem -> FuncItem -> Parser Token Expr
parseInfix' fi prec initial fname@(FuncItem (FunctionName (Placeholder:ss) fnprec) _)
  | prec <= fnprec
  = parseFollowUp fi ss (succ fnprec) >>= (parseInfix fi prec . ExprCall) . createCall fname . (initial:)
  | otherwise
  = empty
parseInfix' _ _ _ _ = empty

parseFollowUp :: [FuncItem] -> [Symbol] -> Prec -> Parser Token [CallItem]
parseFollowUp fi (Symbol s:ss) prec
  = identifierExact s *> parseFollowUp fi ss prec
parseFollowUp fi (Placeholder:ss) prec = do
  e <- placeholder PlaceholderCall <|> (ExprCall <$> parsePrec fi prec)
  (e:) <$> parseFollowUp fi ss prec
parseFollowUp _ [] _ = return []

typeCheck :: [FuncItem] -> Prec -> CallItem -> Parser Token Expr
typeCheck fi prec (ExprCall a) = parseTokenType IsKw >> ExprCall . TypeCheck a <$> typeId >>= parseInfix fi prec
typeCheck _ _ _ = empty

primItem :: [FuncItem] -> Parser Token CallItem
primItem fi = ExprCall <$> prim fi <|> placeholder PlaceholderCall

prim :: [FuncItem] -> Parser Token Expr
prim funcItems = floatExpr <|> intExpr <|> stringExpr <|> parenExpr funcItems <|> structExpr funcItems <|> lambdaExpr funcItems

intExpr :: Parser Token Expr
intExpr = IntegerLit <$> intLiteral

floatExpr :: Parser Token Expr
floatExpr = FloatLit <$> floatLiteral

stringExpr :: Parser Token Expr
stringExpr = StringLit <$> stringLiteral

parenExpr :: [FuncItem] -> Parser Token Expr
parenExpr funcItems = parseTokenType ParenOpen *> parsePrec funcItems lowestPrec <* parseTokenType ParenClose

structExprContent :: [FuncItem] -> Parser Token [Expr]
structExprContent funcItems = structExprContent' <|> (pure <$> parsePrec funcItems lowestPrec) <|> return []
  where
    structExprContent' = liftM2 (:) (parsePrec funcItems lowestPrec) (parseTokenType Comma >> structExprContent funcItems)

structExpr :: [FuncItem] -> Parser Token Expr
structExpr funcItems = StructLit <$> (TypeName <$> identifier <* parseTokenType CurlyOpen) <*> (structExprContent funcItems <* parseTokenType CurlyClose)

arrExpr :: [FuncItem] -> Parser Token Expr
arrExpr funcItems = ArrLit <$> (TypeName <$> identifier <* parseTokenType BracketOpen) <*> (structExprContent funcItems <* parseTokenType BracketClose)

lambdaExpr :: [FuncItem] -> Parser Token Expr
lambdaExpr funcItems = do
  parseTokenType Backslash
  args <- some textIdentifier
  parseTokenType Equal
  body <- parsePrec (map primaryFuncItem args ++ funcItems) lowestPrec
  return $ createLambda args body

createLambda :: [String] -> Expr -> Expr
createLambda args body = foldr Lambda body args

-- Utility functions

createCall :: FuncItem -> [CallItem] -> Expr
createCall = createCallN 0

createCallN :: Int -> FuncItem -> [CallItem] -> Expr
createCallN n fn items = createCall' (findIndex isPlaceholder items)
  where createCall' Nothing = Call (funcName fn) $ [wrap n e l | (n, ExprCall e, l) <- zip3 [0 :: Int ..] items (lazynesses fn)]
        createCall' (Just i) = Lambda argname $ createCallN (succ n) fn $ replaceAt i (ExprCall $ Call (FunctionName [Symbol argname] defaultPrec) []) items
        wrap n e True = Lambda ("lazy " ++ show n) e
        wrap _ e False = e
        argname = "$" ++ show n
        isPlaceholder PlaceholderCall = True
        isPlaceholder _ = False

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n element xs = take n xs ++ [element] ++ drop (n + 1) xs
