{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Main for ekc
--}

module Main (main) where

import ArgParser
import Tokenizer
import EK.Parser
import EK.Compiler

import Data.Maybe (fromMaybe)

import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Control.Monad (when)

readFileOrStdIn :: Maybe String -> IO String
readFileOrStdIn Nothing = getContents
readFileOrStdIn (Just file) = readFile file

writeFileOrStdOut :: Maybe String -> String -> IO ()
writeFileOrStdOut Nothing content = putStrLn content
writeFileOrStdOut (Just file) content = writeFile file content

main :: IO ()
main = do
  args' <- getArgs
  arg <- either (fail . show) return $ parseArguments args'
  let output o = writeFileOrStdOut (argOutput arg) o >> exitSuccess
  content <- readFileOrStdIn $ argInput arg
  tokens <- either (fail . show) return $ tokenizer ("stdin" `fromMaybe` argInput arg) content
  when (argOutputType arg == Just OutputTokens) $ output $ show tokens
  ast <- either (fail . show) return $ parseDocument tokens
  when (argOutputType arg == Just OutputAst) $ output $ show ast
  insts <- either fail return $ compileToVM ast
  putStrLn $ showBytecode insts
  return ()
