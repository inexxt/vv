module Main where

import Lib
import Lambda.SimplyTyped

main :: IO ()
main = print $ typecheck initialEnv (app2 plus one two)
