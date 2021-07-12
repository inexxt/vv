module Config (module Config, lift, asks, return, when) where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad
import System.IO (hPutStrLn, stderr)
import PrettyPrinter
import Eval

data Cfg = Cfg
  { verboseInterpreter :: Bool,
    verboseParser :: Bool,
    niceOutput :: Bool,
    stdlibLoc :: FilePath,
    isRepl :: Bool,
    programFile :: Maybe FilePath
  }
  deriving (Show, Eq)

type IOConfig a = ReaderT Cfg IO a

newCfg :: Cfg
newCfg = Cfg False False True "./stdlib/" False Nothing

debugPrint :: String -> IOConfig ()
debugPrint s = do
  shouldPrint <- asks verboseInterpreter
  when shouldPrint $ printStdout s

debugPrintParser :: String -> IOConfig ()
debugPrintParser s = do
  shouldPrint <- asks verboseParser
  when shouldPrint $ printStdout s

printStdout :: String -> IOConfig ()
printStdout s = lift (putStrLn s)

printStderr :: String -> IOConfig ()
printStderr s = lift $ hPutStrLn stderr s

printOutput :: Expr -> IOConfig ()
printOutput s = do
  shouldPrintNice <- asks niceOutput
  if shouldPrintNice
    then printStdout $ prettyPrint (hideArgs $ nf s) 
    else printStdout $ prettyPrint (nf s)