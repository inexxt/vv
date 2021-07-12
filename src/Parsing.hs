module Parsing where

import Parser.AbsVVSyntax (Program)
import Parser.LexVVSyntax (Token)
import Parser.ParVVSyntax (myLexer, pProgram)
import Parser.PrintVVSyntax (Print, printTree)
import System.Exit (exitFailure)
import Config
import Prelude
import Err

type ParseFun a = [Token] -> Either ErrMsg a

runFile :: (Print a, Show a) => ParseFun a -> FilePath -> IOConfig a
runFile p f = do
  contents <- lift $ readFile f
  run f p contents

run :: (Print a, Show a) => FilePath -> ParseFun a -> String -> IOConfig a
run f p s =
  case p ts of
    Left err -> do
      printStderr "\nParse              Failed...\n"
      printStderr "Tokens:"
      printStderr $ show ts
      printStderr err
      printStderr $ "In file " ++ f
      lift exitFailure
    Right tree -> do
      debugPrintParser "\nParse Successful!"
      debugPrintParser $ "\n[Abstract Syntax]\n\n" ++ show tree
      debugPrintParser $ "\n[Linearized tree]\n\n" ++ printTree tree
      return tree
  where
    ts = myLexer s

runParser :: FilePath -> IOConfig Program
runParser = runFile pProgram