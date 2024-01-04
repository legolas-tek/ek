{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- expression parser for ek
-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module EK.ExprParser
  ( parseExprs
  , TotalStmt
  , PartialStmt
  ) where

import EK.Ast
import Token
import Parser
import EK.TokenParser
import Control.Monad (liftM2)
import Control.Applicative (Alternative(empty))
import Data.Monoid (Alt(..))
import Diagnostic

type TotalStmt = EK.Ast.Stmt Expr
type PartialStmt = EK.Ast.Stmt [Token]

type FuncItem = FunctionName

lowestPrec :: Prec
lowestPrec = 0

primaryPrec :: Prec
primaryPrec = defaultPrec

parseExprs :: [PartialStmt] -> Either Diagnostic [TotalStmt]
parseExprs partials = mapM (parseBody partials) partials

parseBody :: [PartialStmt] -> PartialStmt -> Either Diagnostic TotalStmt
parseBody partials (FuncDef pat body) = FuncDef pat <$> parseExpr (args ++ funcItems partials) body
  where
    args = funcPatternItems pat >>= argFuncItems
    argFuncItems (ArgPattern _ s _) = [FunctionName [Symbol s] primaryPrec]
    argFuncItems PlaceholderPattern = []
    argFuncItems (SymbolPattern _) = []

parseBody _ (ExternDef pat) = return $ ExternDef pat
parseBody _ (AtomDef name) = return $ AtomDef name
parseBody _ (TypeDef name ty) = return $ TypeDef name ty
parseBody _ (StructDef name elems) = return $ StructDef name elems

parseExpr :: [FuncItem] -> [Token] -> Either Diagnostic Expr
parseExpr fi tokens = fst <$> runParser (parsePrec fi lowestPrec <* eof) tokens

funcItems :: [PartialStmt] -> [FuncItem]
funcItems (FuncDef pat _ : xs) = patternToName pat : funcItems xs
funcItems (ExternDef pat : xs) = patternToName pat : funcItems xs
funcItems (AtomDef name : xs) = FunctionName [Symbol name] primaryPrec : funcItems xs
funcItems (_ : xs) = funcItems xs
funcItems [] = []

parsePrec :: [FuncItem] -> Prec -> Parser Token Expr
parsePrec fi prec = primItem fi <|> parsePrefix fi prec >>= parseInfix fi prec

parsePrefix :: [FuncItem] -> Prec -> Parser Token CallItem
parsePrefix fi prec = getAlt (foldMap (Alt . parsePrefix' fi prec) fi) <|> fail "Could not resolve expression"

parsePrefix' :: [FuncItem] -> Prec -> FuncItem -> Parser Token CallItem
parsePrefix' fi prec fname@(FunctionName (Symbol s:ss) fnprec)
  = ExprCall <$> (identifierExact s >> Call fname <$> parseFollowUp fi ss (max prec fnprec))
parsePrefix' _ _ _ = empty

parseInfix :: [FuncItem] -> Prec -> CallItem -> Parser Token Expr
parseInfix fi prec initial = getAlt (foldMap (Alt . parseInfix' fi prec initial) fi) <|> noInfix initial
  where noInfix (ExprCall i) = return i
        noInfix PlaceholderCall = fail "Invalid placeholder"

parseInfix' :: [FuncItem] -> Prec -> CallItem -> FuncItem -> Parser Token Expr
parseInfix' fi prec initial fname@(FunctionName (Placeholder:ss) fnprec)
  | prec <= fnprec
  = parseFollowUp fi ss (succ fnprec) >>= (parseInfix fi prec . ExprCall) . Call fname . (initial:)
  | otherwise
  = empty
parseInfix' _ _ _ _ = empty

parseFollowUp :: [FuncItem] -> [Symbol] -> Prec -> Parser Token [CallItem]
parseFollowUp fi (Symbol s:ss) prec
  = identifierExact s *> parseFollowUp fi ss prec
parseFollowUp fi (Placeholder:ss) prec = do
  e <- parsePrec fi prec -- TODO: use prec from fn
  (ExprCall e :) <$> parseFollowUp fi ss prec
parseFollowUp _ [] _ = return []

primItem :: [FuncItem] -> Parser Token CallItem
primItem fi = ExprCall <$> prim fi <|> placeholder PlaceholderCall

prim :: [FuncItem] -> Parser Token Expr
prim funcItems = intExpr <|> stringExpr <|> parenExpr funcItems <|> structExpr funcItems

intExpr :: Parser Token Expr
intExpr = IntegerLit <$> intLiteral

stringExpr :: Parser Token Expr
stringExpr = StringLit <$> stringLiteral

parenExpr :: [FuncItem] -> Parser Token Expr
parenExpr funcItems = parseTokenType ParenOpen *> parsePrec funcItems lowestPrec <* parseTokenType ParenClose

structExprContent :: [FuncItem] -> Parser Token [Expr]
structExprContent funcItems = structExprContent' <|> (pure <$> parsePrec funcItems lowestPrec) <|> return []
  where
    structExprContent' = liftM2 (:) (parsePrec funcItems lowestPrec) (parseTokenType Comma >> structExprContent funcItems)

structExpr :: [FuncItem] -> Parser Token Expr
structExpr funcItems = StructLit <$> (identifier <* parseTokenType CurlyOpen) <*> (structExprContent funcItems <* parseTokenType CurlyClose)
