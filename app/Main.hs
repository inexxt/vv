module Main where

import App

import Control.Monad
import System.Environment (getArgs)
import System.Exit        (exitFailure, exitSuccess)
import Config
import Err
import Control.Monad.Trans.Reader
import Data.Maybe


usage :: String
usage =
    unlines
      [ "Usage: ./interpreter [OPTIONS] <file>",
        "  --help   Display this help message.",
        "  <file>   File (.vv) defining 'main'.",
        "  -v       Enable verbosity for interpreter",
        "  -vp      Enable verbosity for parser",
        "  -d       Disable nice printing.",
        "  -s <dir> Specify the location of standard library (if not specified, assuming current dir)"
      ]

buildConfig :: [String] -> Cfg -> IO Cfg
buildConfig [] cfg = return cfg
buildConfig ("-v" : as) cfg = buildConfig as cfg {verboseInterpreter = True}
buildConfig ("-vp" : as) cfg = buildConfig as cfg {verboseParser = True}
buildConfig ("-d" : as) cfg = buildConfig as cfg {niceOutput = False}
buildConfig ("--help" : as) cfg = do
    putStrLn usage
    exitSuccess
buildConfig ("-s" : loc : as) cfg = buildConfig as cfg {stdlibLoc = loc}
buildConfig [a] cfg = return $ cfg {programFile = Just a}
buildConfig as _ = do
  fail $ "Unuppported parameters: " ++ show as

main :: IO ()
main = do
    args <- getArgs
    cfg <- buildConfig args newCfg
    case programFile cfg of
        Just fname -> runReaderT (executeFile fname) cfg
        Nothing -> fail "You have to provide filename when running the interpreter"