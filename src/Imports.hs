module Imports where

import Prelude
import Data.List
import Parser.AbsVVSyntax
import Parsing
import Config
import System.FilePath

localSuffix :: String
localSuffix = "_Local"

-- this manages relative/absolute imports
getFilePath :: String -> FilePath -> FilePath
getFilePath f stdlib =
    if localSuffix `isSuffixOf` f then "./" </> take (length f - length localSuffix) f ++ ".vvm"
    else stdlib </> (f ++ ".vvm")

-- Note: I don't check for circular imports
-- gatherImports sorts imports topologically - the order is important!
gatherImports :: FilePath -> IOConfig [(Program, FilePath)]
gatherImports f = do
  p1@(Program imports definitions) <- runParser f
  stdlib <- asks stdlibLoc
  let importFnames = map (\(Import (Ident f)) -> getFilePath f stdlib) imports
  rr <- mapM gatherImports importFnames
  return $ (p1, f) : concat rr