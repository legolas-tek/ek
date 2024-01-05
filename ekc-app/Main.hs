{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Main for ekc
--}

{-# LANGUAGE TupleSections #-}

module Main (main) where

import System.IO
import System.Environment

import Tokenizer
import EK.Parser
import EK.Compiler

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  content <- readFile file
  tokens <- either (fail . show) return $ tokenizer file content
  ast <- either (fail . show) return $ parseDocument tokens
  insts <- either fail return $ compileToVM ast
  putStrLn $ showBytecode insts
  return ()
