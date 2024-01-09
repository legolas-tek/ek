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
import EK.Builtins
import Serialize

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Control.Monad (when)
import EK.Optimizer (optimizeBytecode)

readFileOrStdIn :: Maybe String -> IO String
readFileOrStdIn Nothing = getContents
readFileOrStdIn (Just file) = readFile file

writeFileOrStdOut :: Maybe String -> String -> IO ()
writeFileOrStdOut Nothing content = putStrLn content
writeFileOrStdOut (Just file) content = writeFile file content

save :: Result -> Maybe String -> IO ()
save result Nothing = saveResult result "a.out"
save result (Just file) = saveResult result file

main :: IO ()
main = do
  args' <- getArgs
  arg <- either (fail . show) return $ parseArguments args'
  let output o = writeFileOrStdOut (argOutput arg) o >> exitSuccess
  content <- readFileOrStdIn $ argInput arg
  (tokens, diags) <- either (fail . show) return $ tokenizer ("stdin" `fromMaybe` argInput arg) content
  when (argOutputType arg == Just OutputTokens) $ output $ unlines $ show <$> tokens
  (ast, diags') <- parseDocument tokens
  when (argOutputType arg == Just OutputAst) $ output $ show ast
  mapM_ print (diags ++ diags')
  insts' <- either fail return $ compileToVM ast
  let insts = if argOptimize arg then optimizeBytecode insts' else insts'
  when (argOutputType arg == Just OutputBytecode) $ output $ showBytecode insts
  when (argOutputType arg == Just OutputResult) $ runVM insts
  save insts (argOutput arg)
  return ()
