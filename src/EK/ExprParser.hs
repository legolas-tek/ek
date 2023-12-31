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

type TotalStmt = EK.Ast.Stmt Expr
type PartialStmt = EK.Ast.Stmt [Token]

type FuncItem = FunctionName

lowestPrec :: Prec
lowestPrec = 0

primaryPrec :: Prec
primaryPrec = defaultPrec

parseExprs :: [PartialStmt] -> Either String [TotalStmt]
parseExprs partials = mapM (parseBody partials) partials

parseBody :: [PartialStmt] -> PartialStmt -> Either String TotalStmt
parseBody partials (FuncDef pat body) = FuncDef pat <$> parseExpr (args ++ funcItems partials) body
  where
    args = funcPatternItems pat >>= argFuncItems
    argFuncItems (ArgPattern s _) = [FunctionName [Symbol s] primaryPrec]
    argFuncItems PlaceholderPattern = []
    argFuncItems (SymbolPattern _) = []

parseBody _ (ExternDef pat) = return $ ExternDef pat
parseBody _ (AtomDef name) = return $ AtomDef name
parseBody _ (TypeDef name ty) = return $ TypeDef name ty
parseBody _ (StructDef name elems) = return $ StructDef name elems

parseExpr :: [FuncItem] -> [Token] -> Either String Expr
parseExpr fi tokens = fst <$> runParser (parsePrec fi lowestPrec <* eof) tokens

funcItems :: [PartialStmt] -> [FuncItem]
funcItems (FuncDef pat _ : xs) = patternToName pat : funcItems xs
funcItems (ExternDef pat : xs) = patternToName pat : funcItems xs
funcItems (AtomDef name : xs) = FunctionName [Symbol name] primaryPrec : funcItems xs
funcItems (_ : xs) = funcItems xs
funcItems [] = []

parsePrec :: [FuncItem] -> Int -> Parser Token Expr
parsePrec fi prec = primItem fi <|> parsePrefix fi fi prec >>= parseInfix fi fi prec

parsePrefix :: [FuncItem] -> [FuncItem] -> Int -> Parser Token CallItem
parsePrefix fi (fname@(FunctionName (Symbol s:ss) fnprec):fis) prec
  = (ExprCall <$> (identifierExact s >> Call fname <$> parseFollowUp fi ss (max prec fnprec))) <|> parsePrefix fi fis prec
parsePrefix fi (_:fis) prec = parsePrefix fi fis prec
parsePrefix _ [] _ = fail "Could not resolve expression"

parseInfix :: [FuncItem] -> [FuncItem] -> Int -> CallItem -> Parser Token Expr
parseInfix fi (fname@(FunctionName (Placeholder:ss) fnprec):fis) prec initial
  | prec <= fnprec
  = (parseFollowUp fi ss (succ fnprec) >>= (parseInfix fi fi prec . ExprCall) . Call fname . (initial:))
  <|> parseInfix fi fis prec initial
  | otherwise
  = parseInfix fi fis prec initial
parseInfix fi (_:fis) prec initial = parseInfix fi fis prec initial
parseInfix _ [] _ (ExprCall e) = return e
parseInfix _ [] _ _ = fail "Invalid placeholder"

parseFollowUp :: [FuncItem] -> [Symbol] -> Int -> Parser Token [CallItem]
parseFollowUp fi (Symbol s:ss) prec
  = identifierExact s *> parseFollowUp fi ss prec
parseFollowUp fi (Placeholder:ss) prec = do
  e <- parsePrec fi prec -- TODO: use prec from fn
  (ExprCall e :) <$> parseFollowUp fi ss prec
parseFollowUp _ [] _ = return []

primItem :: [FuncItem] -> Parser Token CallItem
primItem fi = ExprCall <$> prim fi <|> placeholder PlaceholderCall

prim :: [FuncItem] -> Parser Token Expr
prim funcItems = intExpr <|> stringExpr <|> parenExpr funcItems

intExpr :: Parser Token Expr
intExpr = IntegerLit <$> intLiteral

stringExpr :: Parser Token Expr
stringExpr = StringLit <$> stringLiteral

parenExpr :: [FuncItem] -> Parser Token Expr
parenExpr funcItems = parseTokenType ParenOpen *> parsePrec funcItems lowestPrec <* parseTokenType ParenClose
