module Butters.Parser where

import Text.ParserCombinators.Parsec

data Constructor =
  BList [Constructor]
  deriving (Show, Eq)

data TopLevelDefinition =
  DataDef String [String] [Constructor]
  deriving (Show, Eq)

constructorName :: Parser String
constructorName = do
  c <- oneOf ['A'..'Z']
  rest <- many (letter <|> digit)
  return $ c:rest

expression :: Parser Constructor
expression = do
  char '['
  contents <- many expression
  char ']'
  return $ BList contents

constructorCase :: Parser Constructor
constructorCase = do
  char ' '
  char '|'
  char ' '
  cons <- expression
  return $ cons

topLevel :: Parser TopLevelDefinition
topLevel = do
  string "data"
  char ' '
  name <- constructorName
  constructors <- many constructorCase
  return $ DataDef name [] constructors

parseTopLevel = parse topLevel ""
