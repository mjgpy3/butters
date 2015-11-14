module Butters.Parser where

import Text.ParserCombinators.Parsec

data Constructor =
  BList [Constructor]
  deriving (Show, Eq)

data TopLevelDefinition =
  DataDef String [String] [Constructor]
  deriving (Show, Eq)

topLevel :: Parser TopLevelDefinition
topLevel = do
  return $ DataDef "Zero" [] [BList []]

parseTopLevel = parse topLevel ""
