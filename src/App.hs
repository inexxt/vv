module App (module App) where

import Config
import Parser.AbsVVSyntax
import Desugar
import Imports
import Data.List
import Typechecking
import Eval
import PrettyPrinter

executeFile :: FilePath -> IOConfig ()
executeFile f = do
  stdlib <- asks stdlibLoc
  debugPrint $ "Loading standard library from " ++ stdlib ++ "\n"
  allImports <- gatherImports f
  -- this is a tricky place - nub keeps only the first occurence
  -- but! we actually want only the last one, because it will be the first
  -- in the topological order. So we reverse the list twice here
  let (allImportsPrograms, allImportsFilenames) = unzip $ reverse (nub (reverse allImports))
  debugPrint $ "Importing files " ++ show allImportsFilenames
  -- Definitions are appended to the end, we need to have 'main' as the first definition 
  -- hence reverse
  let definitions = concatMap (\(Program _ dfs) -> reverse dfs) allImportsPrograms
  debugPrint $ "Names defined in the program : " ++ show (map definitionName definitions)
  case desugarTopLevel definitions of
    Left err -> fail err
    Right (env, dd) -> do
      debugPrint "Parsed OK!"
      debugPrint $ "Got env: " ++ prettyPrintEnv env
      debugPrint $ "Got term: " ++ prettyPrint (nf dd)
      case typecheck freshEnv dd of
        (Left err) -> printStderr err
        (Right ddd) -> printOutput $ nf dd