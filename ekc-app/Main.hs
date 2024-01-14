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

import System.Environment (getArgs, setEnv, lookupEnv)
import System.Exit (exitSuccess)
import Control.Monad (when)
import EK.Optimizer (optimizeBytecode)
import Data.List (intercalate)

import Control.Monad (void)

readFileOrStdIn :: Maybe String -> IO String
readFileOrStdIn Nothing = getContents
readFileOrStdIn (Just file) = readFile file

writeFileOrStdOut :: Maybe String -> String -> IO ()
writeFileOrStdOut Nothing content = putStrLn content
writeFileOrStdOut (Just file) content = writeFile file content

save :: Result -> Maybe String -> IO ()
save result Nothing = saveResult result "a.out"
save result (Just file) = saveResult result file

addImportsPath :: Maybe [String] -> IO ()
addImportsPath Nothing = setEnv "EK_LIBRARY_PATH" "./"
addImportsPath (Just paths) = do
  oldPath <- fromMaybe "" <$> lookupEnv "EK_LIBRARY_PATH"
  setEnv "EK_LIBRARY_PATH" $ intercalate ":" $ paths ++ [oldPath]

main :: IO ()
main = do
  args' <- getArgs
  arg <- either (fail . show) return $ parseArguments args'
  let output o = writeFileOrStdOut (argOutput arg) o >> exitSuccess
  addImportsPath $ argImportPath arg
  content <- readFileOrStdIn $ argInput arg
  (tokens, diags) <- either (fail . show) return $ tokenizer ("stdin" `fromMaybe` argInput arg) content
  when (argOutputType arg == Just OutputTokens) $ output $ unlines $ show <$> tokens
  (ast, diags') <- parseDocument tokens
  when (argOutputType arg == Just OutputAst) $ output $ unlines $ show <$> ast
  mapM_ print (diags ++ diags')
  insts' <- either fail return $ compileToVM ast
  let insts = if argOptimize arg then optimizeBytecode insts' else insts'
  when (argOutputType arg == Just OutputBytecode) $ output $ showBytecode insts
  when (argOutputType arg == Just OutputResult) $ void $ runVM insts
  save insts (argOutput arg)
