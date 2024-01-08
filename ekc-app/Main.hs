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
import VirtualMachine
import EK.Builtins

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Control.Monad (when)

readFileOrStdIn :: Maybe String -> IO String
readFileOrStdIn Nothing = getContents
readFileOrStdIn (Just file) = readFile file

writeFileOrStdOut :: Maybe String -> String -> IO ()
writeFileOrStdOut Nothing content = putStrLn content
writeFileOrStdOut (Just file) content = writeFile file content

runVM :: Result -> IO ()
runVM res = do
  mainFn <- maybe (fail "No main function") return $ Map.lookup "main" res
  let insts = res <> builtins
  let env = FunctionValue <$> insts
  _ <- exec env [] mainFn []
  return ()

main :: IO ()
main = do
  args' <- getArgs
  arg <- either (fail . show) return $ parseArguments args'
  let output o = writeFileOrStdOut (argOutput arg) o >> exitSuccess
  content <- readFileOrStdIn $ argInput arg
  (tokens, diags) <- either (fail . show) return $ tokenizer ("stdin" `fromMaybe` argInput arg) content
  when (argOutputType arg == Just OutputTokens) $ output $ unlines $ show <$> tokens
  (ast, diags') <- parseDocument tokens
  when (argOutputType arg == Just OutputAst) $ output $ unlines $ show <$> ast
  mapM_ print (diags ++ diags')
  insts <- either fail return $ compileToVM ast
  when (argOutputType arg == Just OutputBytecode) $ output $ showBytecode insts
  when (argOutputType arg == Just OutputResult) $ runVM insts
