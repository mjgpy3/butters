module Main where

import TestParser
import TestTypeChecker

main :: IO ()
main = do
  testParser
  testTypeChecker
