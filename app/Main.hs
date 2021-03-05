module Main where

import Lib
import Lambda.Untyped

main :: IO ()
main = print $ betaEq (app2 plus one two) three
